;; ----------------------
;; 📦 Package Setup
;; ----------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ----------------------
;; Emacs path
;; ----------------------
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))



;; ----------------------
;; 🧠 Better Defaults
;; ----------------------
(use-package better-defaults)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode 1)

;; ----------------------
;; 😈 Evil Mode
;; ----------------------
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1))

(setq evil-kill-on-visual-paste nil)

;; ----------------------
;; 🧠 Which Key
;; ----------------------
(use-package which-key
  :config (which-key-mode))

;; ----------------------
;; 💻 LSP + Python + Ruff
;; ----------------------
(use-package lsp-mode
  :hook ((python-mode . lsp))
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-diagnostics-provider :none))

(use-package lsp-pyright
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))
  :config
  (setq lsp-pyright-typechecking-mode "off"))

(defun my/python-ruff-format ()
  "Format current buffer using ruff --fix without losing buffer edits."
  (when (and (eq major-mode 'python-mode)
             (executable-find "ruff"))
    (let ((tempfile (make-temp-file "ruff" nil ".py"))
          (current-point (point)))
      (write-region nil nil tempfile nil 'silent)
      (call-process "ruff" nil nil nil "--fix" tempfile)
      (save-excursion
        (erase-buffer)
        (insert-file-contents tempfile))
      (goto-char current-point))))
(add-hook 'before-save-hook #'my/python-ruff-format)

;; ----------------------
;; 🔤 Company Mode
;; ----------------------
(use-package company
  :hook (after-init . global-company-mode))

;; ----------------------
;; 🌲 Treemacs
;; ----------------------
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t
        treemacs-no-png-images t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  :bind
  (:map global-map
        ("C-x t" . treemacs)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :after (treemacs magit))

;; ----------------------
;; 🌲 Magit
;; ----------------------
(use-package magit
  :commands magit-status
  :config
  (global-set-key (kbd "C-x g") #'magit-status)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; ----------------------
;; 🧪 Pytest Integration
;; ----------------------
(use-package pytest
  :after python
  :hook (python-mode . (lambda ()
                         (local-set-key (kbd "C-c t a") 'pytest-all)
                         (local-set-key (kbd "C-c t m") 'pytest-module)
                         (local-set-key (kbd "C-c t .") 'pytest-one)
                         (local-set-key (kbd "C-c t d") 'pytest-directory))))

;; ----------------------
;; 🎨 Theme
;; ----------------------
(use-package gruvbox-theme
  :config (load-theme 'gruvbox-dark-soft t))

