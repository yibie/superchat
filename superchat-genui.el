;;; superchat-genui.el --- Whitelist gate for generated VUI trees -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Agent-produced UI is received as text and is never evaluated directly.
;; Callers read one form, pass it through `superchat-genui-validate', expand
;; the small trusted action vocabulary, and only then evaluate the result.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom superchat-genui--allowed-constructors
  '(vui-fragment vui-text vui-button vui-strong vui-italic vui-muted
    vui-heading vui-success vui-warning vui-error vui-code)
  "VUI constructors that generated UI may call.

Only these symbols may occur in function position.  Stateful or
side-effecting VUI forms such as `vui-defcomponent', `vui-use-effect', and
`vui-use-async' are intentionally absent."
  :type '(repeat symbol)
  :group 'superchat)

(defconst superchat-genui--max-depth 128
  "Maximum generated-UI list depth accepted by the validator.")

(defconst superchat-genui--max-nodes 10000
  "Maximum number of cons cells visited by one validation pass.")

(defvar superchat-genui-actions
  '((cancel-agent . superchat-genui--build-cancel-agent))
  "Trusted generated-UI actions as an alist of (NAME . BUILDER).

Builders are package code, not values supplied by the agent.  A builder
receives the validated action plist and returns the callback installed on a
VUI event handler.")

(defun superchat-genui-action-names ()
  "Return the names currently available to generated UI actions."
  (mapcar (lambda (entry) (symbol-name (car entry)))
          superchat-genui-actions))

(defun superchat-genui--plain-string-p (value)
  "Return non-nil when VALUE is a string with no text properties."
  (and (stringp value)
       (cl-loop for position from 0 to (length value)
                never (text-properties-at position value))))

(defun superchat-genui--literal-p (value)
  "Return non-nil when VALUE is a safe, self-evaluating literal."
  (or (superchat-genui--plain-string-p value)
      (numberp value)
      (eq value t)
      (null value)
      (keywordp value)))

(defun superchat-genui--constructor-p (value)
  "Return non-nil when VALUE is an exact whitelisted constructor symbol."
  ;; `memq' deliberately rejects an uninterned symbol that merely prints like
  ;; a whitelisted name.
  (and (symbolp value)
       (memq value superchat-genui--allowed-constructors)))

(defun superchat-genui--event-key-p (value)
  "Return non-nil when VALUE is a VUI event keyword."
  (and (keywordp value)
       (string-prefix-p ":on-" (symbol-name value))))

(defun superchat-genui--label (value)
  "Return a bounded diagnostic label for VALUE."
  (cond ((symbolp value) (symbol-name value))
        ((stringp value)
         (format "string[%d]" (length value)))
        ((consp value) "list")
        (t (format "%s" (type-of value)))))

(defun superchat-genui--reject (format-string &rest args)
  "Build a readable validator rejection string."
  (concat "genui: " (apply #'format format-string args)))

(defun superchat-genui--seen-or-mark (cell seen)
  "Return non-nil when CELL was seen, otherwise mark it in SEEN."
  (if (gethash cell seen)
      t
    (puthash cell t seen)
    nil))

(defun superchat-genui--budget-exceeded-p (seen)
  "Return non-nil when validation has visited too many cons cells."
  (>= (hash-table-count seen) superchat-genui--max-nodes))

(defun superchat-genui--action-entry (name)
  "Return the trusted action entry for NAME, or nil.
Strings are accepted for callers constructing forms programmatically; an
uninterned symbol is not interned implicitly and therefore cannot bypass the
identity check." 
  (if (stringp name)
      (cl-find-if (lambda (entry)
                    (and (symbolp (car entry))
                         (string= name (symbol-name (car entry)))))
                  superchat-genui-actions)
    (assq name superchat-genui-actions)))

(cl-defun superchat-genui--validate-action (form depth seen)
  "Validate an event handler FORM at DEPTH using SEEN cons cells."
  (cond
   ((>= depth superchat-genui--max-depth)
    (superchat-genui--reject "超过最大嵌套深度"))
   ((not (consp form))
    (superchat-genui--reject "事件处理器必须是 (action NAME ...)，得到 %s"
                             (superchat-genui--label form)))
   ((superchat-genui--seen-or-mark form seen)
    (superchat-genui--reject "检测到循环或重复的事件处理器"))
   ((superchat-genui--budget-exceeded-p seen)
    (superchat-genui--reject "UI 形式过大"))
   ((not (eq (car form) 'action))
    (superchat-genui--reject "事件处理器必须使用 action，不允许的形式: %s"
                             (superchat-genui--label (car form))))
   (t
    (let ((name (cadr form))
          (rest (cddr form)))
      (cond
       ((not (or (symbolp name) (stringp name)))
        (superchat-genui--reject "动作名必须是符号或字符串"))
       ((null (superchat-genui--action-entry name))
        (superchat-genui--reject "不允许的动作: %s"
                                 (superchat-genui--label name)))
       (t
        (while (consp rest)
          (when (superchat-genui--seen-or-mark rest seen)
            (cl-return-from superchat-genui--validate-action
              (superchat-genui--reject "检测到循环或重复的动作参数")))
          (when (superchat-genui--budget-exceeded-p seen)
            (cl-return-from superchat-genui--validate-action
              (superchat-genui--reject "UI 形式过大")))
          (let ((key (pop rest)))
            (unless (keywordp key)
              (cl-return-from superchat-genui--validate-action
                (superchat-genui--reject "动作参数键必须是 keyword")))
            (unless (consp rest)
              (cl-return-from superchat-genui--validate-action
                (superchat-genui--reject "动作参数缺少值")))
            (let ((value (pop rest)))
              (unless (superchat-genui--literal-p value)
                (cl-return-from superchat-genui--validate-action
                  (superchat-genui--reject
                   "动作参数 %s 只能是字面量"
                   (superchat-genui--label key)))))))
        (unless (null rest)
          (superchat-genui--reject "动作参数列表必须是 proper list"))))))))

(cl-defun superchat-genui--validate-args (args depth seen)
  "Validate constructor ARGS at DEPTH using SEEN cons cells.
Only event keyword values receive the special action grammar; every other
value is recursively checked as a literal or another whitelisted vnode."
  (let ((rest args)
        (result nil))
    (while (consp rest)
      (when (superchat-genui--seen-or-mark rest seen)
        (cl-return-from superchat-genui--validate-args
          (superchat-genui--reject "检测到循环或重复的构造参数")))
      (when (superchat-genui--budget-exceeded-p seen)
        (cl-return-from superchat-genui--validate-args
          (superchat-genui--reject "UI 形式过大")))
      (let ((item (pop rest)))
        (cond
         ((keywordp item)
          (if (not (consp rest))
              (setq result
                    (superchat-genui--reject "属性 %s 缺少值"
                                             (superchat-genui--label item)))
            (let ((value (pop rest)))
              (setq result
                    (if (superchat-genui--event-key-p item)
                        (superchat-genui--validate-action
                         value (1+ depth) seen)
                      (unless (superchat-genui--literal-p value)
                        (superchat-genui--reject
                         "属性 %s 的值只能是字面量"
                         (superchat-genui--label item))))))))
         (t
          (setq result (superchat-genui--validate-node item
                                                       (1+ depth) seen)))))
      (when result
        (cl-return-from superchat-genui--validate-args result)))
    (if (null rest)
        nil
      (superchat-genui--reject "构造参数必须是 proper list"))))

(defun superchat-genui--validate-node (node depth seen)
  "Validate one generated-UI NODE at DEPTH using SEEN cons cells."
  (cond
   ((superchat-genui--literal-p node) nil)
   ((>= depth superchat-genui--max-depth)
    (superchat-genui--reject "超过最大嵌套深度"))
   ((not (consp node))
    (superchat-genui--reject "不允许的形式: %s"
                             (superchat-genui--label node)))
   ((superchat-genui--seen-or-mark node seen)
    (superchat-genui--reject "检测到循环或重复的 UI 形式"))
   ((superchat-genui--budget-exceeded-p seen)
    (superchat-genui--reject "UI 形式过大"))
   ((not (superchat-genui--constructor-p (car node)))
    (superchat-genui--reject "不允许的形式: %s"
                             (superchat-genui--label (car node))))
   (t
    (superchat-genui--validate-args (cdr node) depth seen))))

;;;###autoload
(defun superchat-genui-validate (form)
  "Return t when FORM is safe generated UI, otherwise a reason string.

This function is pure with respect to the action table: action builders are
not called while validating.  Callers must not evaluate FORM unless this
returns exactly t."
  (let ((seen (make-hash-table :test 'eq)))
    (or (superchat-genui--validate-node form 0 seen)
        t)))

(defun superchat-genui--expand-action (form depth)
  "Expand a validated action FORM into a quoted trusted callback."
  (ignore depth)
  (let* ((name (cadr form))
         (entry (superchat-genui--action-entry name))
         (params (cddr form)))
    (condition-case err
        (let ((builder (cdr entry)))
          (list 'quote
                (funcall builder params)))
      (error
       (superchat-genui--reject "动作 %s 参数无效: %s"
                                (superchat-genui--label name)
                                (error-message-string err))))))

(defun superchat-genui--expand-args (args depth)
  "Expand validated constructor ARGS at DEPTH."
  (let ((rest args)
        (out nil))
    (while (consp rest)
      (let ((item (pop rest)))
        (cond
         ((keywordp item)
          (push item out)
          (let ((value (pop rest)))
            (push (if (superchat-genui--event-key-p item)
                      (superchat-genui--expand-node value (1+ depth) t)
                    value)
                  out)))
         (t
          (push (superchat-genui--expand-node item (1+ depth) nil) out)))))
    (nreverse out)))

(defun superchat-genui--expand-node (node depth &optional action-context)
  "Expand validated NODE at DEPTH.
ACTION-CONTEXT is non-nil only for an event-handler value."
  (cond
   ((superchat-genui--literal-p node) node)
   ((and action-context (consp node) (eq (car node) 'action))
    (superchat-genui--expand-action node depth))
   ((consp node)
    (cons (car node) (superchat-genui--expand-args (cdr node) depth)))
   (t node)))

;;;###autoload
(defun superchat-genui-expand-actions (form)
  "Validate FORM and replace action forms with trusted callback closures.
Returns the transformed form, or the same readable rejection string used by
`superchat-genui-validate'.  No evaluation occurs here."
  (let ((validation (superchat-genui-validate form)))
    (if (stringp validation)
        validation
      (condition-case err
          (superchat-genui--expand-node form 0)
        (error (superchat-genui--reject "%s"
                                       (error-message-string err)))))))

(defun superchat-genui--build-cancel-agent (params)
  "Build the trusted callback for the `cancel-agent' action PARAMS."
  (let ((id (plist-get params :id))
        (keys (cl-loop for (key _value) on params by #'cddr collect key)))
    (unless (and (= (length keys) 1) (eq (car keys) :id))
      (error "cancel-agent accepts only :id"))
    (unless (and (superchat-genui--plain-string-p id)
                 (not (string-empty-p id)))
      (error "cancel-agent requires a non-empty string :id"))
    (let ((target-id id))
      (lambda ()
        (if (fboundp 'superchat-subagent-cancel)
            (superchat-subagent-cancel target-id)
          "Error: sub-agent module is not loaded.")))))

(provide 'superchat-genui)

;;; superchat-genui.el ends here
