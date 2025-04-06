;; init.el --- Optimized Emacs Configuration

;; --------------------------------------
;; Package Management (Optimized)
;; --------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; --------------------------------------
;; Load Paths (Auto-Loading)
;; --------------------------------------

(let ((default-directory "~/.emacs.d/site/"))
  (normal-top-level-add-subdirs-to-load-path))

;; --------------------------------------
;; Basic Customization
;; --------------------------------------

(set-face-attribute 'default nil :height 150)  ; Larger font
(global-hl-line-mode 1)                        ; Highlight current line
(delete-selection-mode nil)                    ; Don't delete selected text on type
(electric-pair-mode 1)                         ; Auto-pair brackets/quotes (replaces autopair)
(global-display-line-numbers-mode 1)           ; Line numbers

;; Safely load custom-file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; --------------------------------------
;; Package Configuration (Using use-package)
;; --------------------------------------

(use-package better-defaults)

(use-package neotree
  :bind ("<f8>" . neotree-toggle)
  :config
  (setq neo-smart-open t))

(setq evil-want-keybinding nil)  ; Critical fix for evil-collection

(use-package evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package elpy
  :init (elpy-enable)
  :bind ("M-*" . pop-tag-mark))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-pycheckers
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(use-package pyenv-mode)
(use-package hy-mode)

(use-package lsp-mode
  :hook ((python-mode . lsp)
         (go-mode . lsp))  ; Enable LSP for Go
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-snippet nil)  ;; Disable snippets (optional)
  :config
  (lsp-enable-which-key-integration t))

;; Enable Ruff LSP (for linting and formatting)
(use-package lsp-ruff
  :ensure nil  ;; Do not try to install it from a package repository.
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (lsp)))
  :config
  (setq lsp-ruff-lsp-server-command '("ruff-lsp")))

;; Enable Python LSP Server (Pylsp) with Rope
(use-package lsp-pylsp
  :ensure nil  ;; Prevent use-package from trying to install a package that doesn’t exist.
  :after lsp-mode
  :config
  (setq lsp-pylsp-plugins-ruff-enabled t
        lsp-pylsp-plugins-ruff-format t
        lsp-pylsp-plugins-mypy-enabled t
        lsp-pylsp-plugins-black-enabled t
        lsp-pylsp-plugins-rope-completion-enabled t
        lsp-pylsp-plugins-rope-refactoring-enabled t))

;; --------------------------------------
;; Go Mode Configuration
;; --------------------------------------

(use-package go-mode
  :hook ((before-save . gofmt-before-save))
  :config
  (setq gofmt-command "gofmt"))

(use-package company
  :hook (go-mode . company-mode))

(use-package flycheck
  :hook (go-mode . flycheck-mode))

(use-package lsp-mode
  :hook (go-mode . lsp)
  :commands lsp)

(use-package dap-mode
  :after lsp-mode
  :config (require 'dap-dlv-go))

;; --------------------------------------
;; Keybindings
;; --------------------------------------

;; Window navigation
(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)
(global-set-key (kbd "M-3") 'windmove-up)
(global-set-key (kbd "M-4") 'windmove-down)

;; Magit (Git integration)
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)
         ("C-x M-b" . magit-blame)))

;; elpy
(define-key elpy-mode-map (kbd "C-c g") #'elpy-goto-definition)

;; --------------------------------------
;; Custom Functions
;; --------------------------------------

(defun inner-open-project (directory)
  "Open a project in DIRECTORY and activate its Python environment."
  (interactive "DOpen project: ")
  (pyvenv-activate directory)
  (let ((project-name (car (last (split-string directory "/")))))
    (find-file (format "~/dev/%s/setup.py" project-name))))

(defun open-project ()
  "Interactively open a project from ~/.pyenv/versions."
  (interactive)
  (let ((default-directory "~/.pyenv/versions/"))
    (call-interactively 'inner-open-project)))

;; --------------------------------------
;; Final Settings
;; --------------------------------------

(put 'downcase-region 'disabled nil)  ; Enable case commands
(put 'upcase-region 'disabled nil)

