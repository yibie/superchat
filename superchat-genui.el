;;; superchat-genui.el --- TextUI data gate for generated interfaces -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Agent-produced UI is received as text and never evaluated.  Callers read
;; one form (or explicitly wrap several), validate it against the TextUI
;; element grammar, expand the trusted action vocabulary into closures, and
;; hand the resulting frame (pure data plus closures) to TextUI.
;;
;; Model-facing grammar:
;;
;;   element := (:type :text :value STR)
;;           |  (:type :flex :direction :row|:column :children (ELEMENT...))
;;           |  (:type :grid :columns N :min-column-width N :children (ELEMENT...))
;;           |  (:type button :label STR :action (action NAME ARGS...))
;;
;; `button' is the sole native widget.el type; it renders through TextUI's
;; native-widget path.  `(action NAME ...)' forms expand to trusted package
;; closures installed on the button's :notify -- agent data never becomes
;; executable code.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar superchat-buffer-name)
(defvar superchat--prompt-start)
(declare-function superchat-send-input "superchat-dispatcher" ())
(declare-function textui-open "textui" (name render-function &optional initial-state))
(declare-function textui-update "textui" (buffer updater &rest keys))

(defconst superchat-genui--allowed-types '(:text :flex :grid)
  "Keyword element types that generated UI may use.")

(defconst superchat-genui--allowed-widgets '(button)
  "Native widget.el types that generated UI may use.")

(defconst superchat-genui--max-depth 128
  "Maximum generated-UI list depth accepted by the validator.")

(defconst superchat-genui--max-nodes 10000
  "Maximum number of cons cells visited by one validation pass.")

(defcustom superchat-genui-buffer-name "*superchat-generated-ui*"
  "Name of the TextUI buffer used by `render_ui'."
  :type 'string
  :group 'superchat)

(defvar superchat-genui-actions
  '((cancel-agent . superchat-genui--build-cancel-agent)
    (send-to-chat . superchat-genui--build-send-to-chat))
  "Trusted generated-UI actions as an alist of (NAME . BUILDER).

Builders are package code, not values supplied by the agent.  A builder
receives the validated action plist and returns the closure installed as a
button's :notify.")

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
An uninterned symbol is not interned implicitly and therefore cannot bypass
the identity check."
  (and (symbolp name)
       (assq name superchat-genui-actions)))

(cl-defun superchat-genui--validate-action (form depth seen)
  "Validate an action FORM at DEPTH using SEEN cons cells."
  (cond
   ((>= depth superchat-genui--max-depth)
    (superchat-genui--reject "maximum nesting depth exceeded"))
   ((not (consp form))
    (superchat-genui--reject "action must be (action NAME ...), got %s"
                             (superchat-genui--label form)))
   ((not (proper-list-p form))
    (superchat-genui--reject "action must be a proper list"))
   ((superchat-genui--seen-or-mark form seen)
    (superchat-genui--reject "circular or repeated action"))
   ((superchat-genui--budget-exceeded-p seen)
    (superchat-genui--reject "UI form too large"))
   ((not (eq (car form) 'action))
    (superchat-genui--reject "action must use action, got form: %s"
                             (superchat-genui--label (car form))))
   (t
    (let ((name (cadr form))
          (rest (cddr form)))
      (cond
       ((not (symbolp name))
        (superchat-genui--reject "action name must be a symbol"))
       ((null (superchat-genui--action-entry name))
        (superchat-genui--reject "action not allowed: %s"
                                 (superchat-genui--label name)))
       (t
        (while (consp rest)
          (when (superchat-genui--seen-or-mark rest seen)
            (cl-return-from superchat-genui--validate-action
              (superchat-genui--reject "circular or repeated action args")))
          (when (superchat-genui--budget-exceeded-p seen)
            (cl-return-from superchat-genui--validate-action
              (superchat-genui--reject "UI form too large")))
          (let ((key (pop rest)))
            (unless (keywordp key)
              (cl-return-from superchat-genui--validate-action
                (superchat-genui--reject "action arg key must be a keyword")))
            (unless (consp rest)
              (cl-return-from superchat-genui--validate-action
                (superchat-genui--reject "action arg missing a value")))
            (let ((value (pop rest)))
              (unless (superchat-genui--literal-p value)
                (cl-return-from superchat-genui--validate-action
                  (superchat-genui--reject
                   "action arg %s must be a literal"
                   (superchat-genui--label key)))))))
        (unless (null rest)
          (superchat-genui--reject "action args must be a proper list"))))))))

(cl-defun superchat-genui--validate-children (value depth seen)
  "Validate CHILDREN as a proper list of element nodes."
  (unless (proper-list-p value)
    (cl-return-from superchat-genui--validate-children
      (superchat-genui--reject "children must be a proper list")))
  (let ((rest value))
    (while (consp rest)
      (when (superchat-genui--seen-or-mark rest seen)
        (cl-return-from superchat-genui--validate-children
          (superchat-genui--reject "circular or repeated children")))
      (when (superchat-genui--budget-exceeded-p seen)
        (cl-return-from superchat-genui--validate-children
          (superchat-genui--reject "UI form too large")))
      (let ((result (superchat-genui--validate-node
                     (pop rest) (1+ depth) seen)))
        (when result
          (cl-return-from superchat-genui--validate-children result))))
    (unless (null rest)
      (superchat-genui--reject "children must be a proper list"))))

(cl-defun superchat-genui--validate-keyword-element
    (type element depth seen)
  "Validate keyword ELEMENT of TYPE."
  (pcase type
    (:text
     (let ((cursor element))
       (while cursor
         (unless (memq (car cursor) '(:type :value))
           (cl-return-from superchat-genui--validate-keyword-element
             (superchat-genui--reject "unknown :text property: %s"
                                      (superchat-genui--label (car cursor)))))
         (setq cursor (cddr cursor))))
     (unless (superchat-genui--plain-string-p (plist-get element :value))
       (superchat-genui--reject ":text :value must be a string")))
    (:flex
     (let ((cursor element))
       (while cursor
         (unless (memq (car cursor) '(:type :direction :children))
           (cl-return-from superchat-genui--validate-keyword-element
             (superchat-genui--reject "unknown :flex property: %s"
                                      (superchat-genui--label (car cursor)))))
         (setq cursor (cddr cursor))))
     (let ((direction (plist-get element :direction)))
       (if (memq direction '(:row :column))
           (superchat-genui--validate-children
            (plist-get element :children) (1+ depth) seen)
         (superchat-genui--reject
          ":flex :direction must be :row or :column"))))
    (:grid
     (let ((cursor element))
       (while cursor
         (unless (memq (car cursor) '(:type :columns :min-column-width
                                       :children))
           (cl-return-from superchat-genui--validate-keyword-element
             (superchat-genui--reject "unknown :grid property: %s"
                                      (superchat-genui--label (car cursor)))))
         (setq cursor (cddr cursor))))
     (dolist (property '(:columns :min-column-width))
       (let ((value (plist-get element property)))
         (unless (and (integerp value) (> value 0))
           (cl-return-from superchat-genui--validate-keyword-element
             (superchat-genui--reject "%s must be a positive integer" property)))))
     (superchat-genui--validate-children
      (plist-get element :children) (1+ depth) seen))))

(cl-defun superchat-genui--validate-widget-element
    (element depth seen)
  "Validate a native widget ELEMENT (button)."
  (let ((cursor element))
    (while cursor
      (unless (memq (car cursor) '(:type :label :action))
        (cl-return-from superchat-genui--validate-widget-element
          (superchat-genui--reject "unknown button property: %s"
                                   (superchat-genui--label (car cursor)))))
      (setq cursor (cddr cursor))))
  (let ((label (plist-get element :label))
        (action (plist-get element :action)))
    (cond
     ((not (and (superchat-genui--plain-string-p label)
                (not (string-empty-p (string-trim label)))))
      (superchat-genui--reject "button :label must be a non-empty string"))
     ((null action)
      (superchat-genui--reject "button requires :action"))
     (t
      (superchat-genui--validate-action action (1+ depth) seen)))))

(defun superchat-genui--validate-node (node depth seen)
  "Validate one generated-UI NODE at DEPTH using SEEN cons cells."
  (cond
   ((superchat-genui--literal-p node) nil)
   ((>= depth superchat-genui--max-depth)
    (superchat-genui--reject "maximum nesting depth exceeded"))
   ((not (consp node))
    (superchat-genui--reject "form not allowed: %s"
                             (superchat-genui--label node)))
   ((superchat-genui--seen-or-mark node seen)
    (superchat-genui--reject "circular or repeated UI form"))
   ((superchat-genui--budget-exceeded-p seen)
    (superchat-genui--reject "UI form too large"))
   ((not (proper-list-p node))
    (superchat-genui--reject "element must be a proper list"))
   (t
    (let ((type (plist-get node :type)))
      (cond
       ((memq type superchat-genui--allowed-types)
        (superchat-genui--validate-keyword-element
         type node depth seen))
       ((memq type superchat-genui--allowed-widgets)
        (superchat-genui--validate-widget-element node depth seen))
       (t
        (superchat-genui--reject "element type not allowed: %s"
                                 (superchat-genui--label type))))))))

(defun superchat-genui-validate (form)
  "Return t when FORM is safe generated UI, otherwise a reason string.

This function is pure with respect to the action table: action builders are
not called while validating."
  (let ((seen (make-hash-table :test 'eq)))
    (or (superchat-genui--validate-node form 0 seen)
        t)))

(defun superchat-genui--expand-action (form)
  "Expand a validated action FORM into the trusted button callback."
  (let* ((name (cadr form))
         (entry (superchat-genui--action-entry name))
         (params (cddr form))
         (builder (cdr entry)))
    ;; Let builder errors unwind to `superchat-genui-expand-actions'.
    (funcall builder params)))

(cl-defun superchat-genui--expand-node (node depth)
  "Expand validated NODE into TextUI frame data.
Agent data never becomes executable code: only action positions receive
closures built by the trusted registry."
  (cond
   ((superchat-genui--literal-p node) node)
   ((not (consp node)) node)
   (t
    (let ((type (plist-get node :type)))
      (cond
       ((eq type :text) (copy-sequence node))
       ((or (eq type :flex) (eq type :grid))
        (let ((copy (copy-sequence node)))
          (plist-put
           copy :children
           (mapcar (lambda (child)
                     (superchat-genui--expand-node child (1+ depth)))
                   (plist-get node :children)))))
       ((memq type superchat-genui--allowed-widgets)
        (list :type 'push-button
              :tag (plist-get node :label)
              :notify
              (superchat-genui--expand-action (plist-get node :action))))
       (t node))))))

(defun superchat-genui-expand-actions (form)
  "Validate FORM and expand it into TextUI frame data.
Returns the expanded element, or the same readable rejection string used by
`superchat-genui-validate'.  No evaluation occurs here."
  (let ((validation (superchat-genui-validate form)))
    (if (stringp validation)
        validation
      (condition-case err
          (superchat-genui--expand-node form 0)
        (error (superchat-genui--reject "%s"
                                       (error-message-string err)))))))

(defun superchat-genui--safe-buffer-name ()
  "Return the generated-UI buffer name unless it aliases the chat surface."
  (let* ((name (or superchat-genui-buffer-name
                   "*superchat-generated-ui*"))
         (existing (and (stringp name) (get-buffer name))))
    (if (or (not (stringp name))
            (string-empty-p name)
            (and (boundp 'superchat-buffer-name)
                 (equal name superchat-buffer-name))
            (and existing
                 (boundp 'superchat-mode)
                 (buffer-local-value 'superchat-mode existing)))
        (generate-new-buffer-name "*superchat-generated-ui*")
      name)))

(defun superchat-genui--display (buffer)
  "Display BUFFER beside the chat surface in a right side window.
True in-buffer inlining is blocked upstream (textui regions require
textui-mode); the side window keeps the UI on screen with the chat.
TextUI's own first display may pick an ordinary window, so drop
non-side windows before opening the side window — otherwise the same
buffer stacks into two visible windows."
  (when (not noninteractive)
    (dolist (win (get-buffer-window-list buffer nil t))
      (unless (or (eq (window-parameter win 'window-side) 'right)
                  (eq win (selected-window)))
        (delete-window win))))
  (display-buffer
   buffer
   '((display-buffer-in-side-window)
     (side . right)
     (window-width . 0.42))))

(defun superchat-genui-render (element)
  "Render expanded ELEMENT in the generated-UI TextUI buffer.

Returns the buffer name, or an error string.  The buffer is created with
`textui-open' on first use and updated with `textui-update' afterwards."
  (if (not (or (featurep 'textui) (require 'textui nil t)))
      "Error: textui.el not available"
    (condition-case err
        (let* ((buffer-name (superchat-genui--safe-buffer-name))
               (frame (list element))
               (existing (get-buffer buffer-name)))
          (if (and existing
                   (with-current-buffer existing
                     (derived-mode-p 'textui-mode)))
              (progn
                (textui-update existing (lambda (_old) frame))
                (superchat-genui--display existing)
                buffer-name)
            (let ((buffer (textui-open buffer-name
                                     (lambda (_width) textui-state) frame)))
              (superchat-genui--display buffer)
              buffer-name)))
      (error (format "Error: %s" (error-message-string err))))))

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
      (lambda (&rest _event)
        (if (fboundp 'superchat-subagent-cancel)
            (superchat-subagent-cancel target-id)
          "Error: sub-agent module is not loaded.")))))

(defun superchat-genui--build-send-to-chat (params)
  "Build the trusted callback for the `send-to-chat' action PARAMS.
The generated UI supplies a fixed prompt; it cannot supply a function or
choose an arbitrary buffer.  A non-empty existing draft is left untouched so
a button cannot silently overwrite user input."
  (let ((text (plist-get params :text))
        (keys (cl-loop for (key _value) on params by #'cddr collect key)))
    (unless (and (= (length keys) 1) (eq (car keys) :text))
      (error "send-to-chat accepts only :text"))
    (unless (and (superchat-genui--plain-string-p text)
                 (not (string-empty-p (string-trim text)))
                 (<= (length text) 8000))
      (error "send-to-chat requires a non-empty string :text (max 8000 chars)"))
    (let ((prompt (string-trim text)))
      (lambda (&rest _event)
        (condition-case err
            (if (not (fboundp 'superchat-send-input))
                "Error: superchat is not loaded."
              (let* ((buffer-name (and (boundp 'superchat-buffer-name)
                                       superchat-buffer-name))
                     (buffer (and (stringp buffer-name)
                                  (get-buffer buffer-name))))
                (if (not (buffer-live-p buffer))
                    "Error: superchat chat buffer is not available."
                  (with-current-buffer buffer
                    (if (not (and (boundp 'superchat--prompt-start)
                                  (markerp superchat--prompt-start)
                                  (marker-position superchat--prompt-start)))
                        "Error: superchat prompt is not ready."
                      (let ((inhibit-read-only t)
                            (draft (string-trim
                                    (buffer-substring-no-properties
                                     superchat--prompt-start (point-max)))))
                        (if (not (string-empty-p draft))
                            "Error: chat prompt already contains text."
                          (goto-char superchat--prompt-start)
                          (delete-region (point) (point-max))
                          (insert prompt)
                          (superchat-send-input)
                          "Sent generated UI selection to chat.")))))))
          (error (format "Error: %s" (error-message-string err))))))))

(provide 'superchat-genui)

;;; superchat-genui.el ends here
