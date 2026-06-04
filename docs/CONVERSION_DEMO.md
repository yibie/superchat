# Skill 格式转换演示

## 场景 1：从标准格式导入

假设你有一个 OpenAI Codex skill：

```
~/.agents/skills/my-skill/
├── SKILL.md
└── references/
    └── doc.md
```

导入到 Superchat：

```elisp
M-x superchat-skills-standard-import
Source directory: ~/.agents/skills/

;; 结果：
;; 创建 ~/.emacs.d/superchat/skills/my-skill.md
;; 可直接使用 >my-skill
```

## 场景 2：导出到标准格式

将 Superchat skill 导出供其他平台使用：

```elisp
M-x superchat-skills-standard-export
Skill to export: code-review
Target directory: ~/.agents/skills/

;; 结果：
;; 创建 ~/.agents/skills/code-review/SKILL.md
```

## 场景 3：同时支持两种格式

```elisp
;; 配置
(setq superchat-skills-directory "~/.emacs.d/superchat/skills/")
(setq superchat-skills-standard-directories
      '("~/.agents/skills/"
        "~/projects/my-project/.agents/skills/"))

(superchat-skills-standard-initialize)

;; 现在可以同时使用：
;; - 原生 skills: >code-review
;; - 标准 skills: >my-codex-skill
```

## 查看所有可用 Skills

```elisp
M-x superchat-skills-standard-list

;; 显示：
;; Standard Skills (3 found):
;; • code-review
;;   Description: Review code quality
;;   Directory: ~/.agents/skills/code-review/
```

## 诊断工具

```elisp
M-x superchat-skills-diagnose

;; 显示：
;; === Agentic Skills Diagnostics ===
;; 
;; Configuration:
;;   Skills directory: ~/.emacs.d/superchat/skills/
;;   Standard format support: enabled
;;   Standard directories: ~/.agents/skills/
;; 
;; Available Skills (5):
;;   - code-review (native)
;;   - planning (native)
;;   - my-codex-skill (standard)
```
