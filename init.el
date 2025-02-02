;; init.el --- Emacs configuration

;; --------------------------------------
;; Package Management
;; --------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Define and install packages
(defvar my-packages
  '(better-defaults
    material-theme
    neotree
    autopair
    elpy
    pyenv-mode
    hy-mode
    evil
    flycheck
    flycheck-pycheckers))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; --------------------------------------
;; Load Paths (for custom files/packages)
;; --------------------------------------

;; Paths for local/custom packages
(add-to-list 'load-path "~/.emacs.d/elpa/autopair-20160304.1237")
(add-to-list 'load-path "~/.emacs.d/site/better-defaults")
(add-to-list 'load-path "~/.emacs.d/site/apo")   ; For blog.el
(add-to-list 'load-path "~/.emacs.d/site/hy-mode")
(add-to-list 'load-path "~/.emacs.d/site/flymake-mypy")

;; --------------------------------------
;; Basic Customization
;; --------------------------------------

(set-face-attribute 'default nil :height 150)  ; Larger font
(global-hl-line-mode 1)                        ; Highlight current line
(delete-selection-mode nil)                    ; Don't delete selected text on type
(autopair-global-mode)                         ; Auto-pair brackets/quotes

;; Safely load custom-file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; --------------------------------------
;; Keybindings
;; --------------------------------------

;; Window navigation
(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)
(global-set-key (kbd "M-3") 'windmove-up)
(global-set-key (kbd "M-4") 'windmove-down)

;; Neotree (file explorer)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)  ; Auto-open files on selection

;; Magit (Git integration)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x M-b") 'magit-blame)

;; Occur (search within buffer)
(global-set-key (kbd "C-c o") 'occur)

;; --------------------------------------
;; Mode Configuration
;; --------------------------------------

;; Evil Mode (Vim keybindings) - LOAD FIRST!
(require 'evil)
(evil-mode 1)

;; Neotree (file explorer)
(require 'neotree)
(evil-define-key 'normal neotree-mode-map
  (kbd "TAB") 'neotree-enter
  (kbd "SPC") 'neotree-quick-look
  (kbd "q")   'neotree-hide
  (kbd "RET") 'neotree-enter
  (kbd "g")   'neotree-refresh
  (kbd "n")   'neotree-next-line
  (kbd "p")   'neotree-previous-line
  (kbd "A")   'neotree-stretch-toggle
  (kbd "H")   'neotree-hidden-file-toggle)

;; Elpy (Python IDE)
(require 'elpy)
(elpy-enable)
(global-set-key (kbd "M-*") 'pop-tag-mark)  ; Jump back from definitions

;; Flycheck (syntax checking)
(require 'flycheck)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)
(require 'flycheck-pycheckers)
(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; Text/Programming Modes
(add-hook 'text-mode-hook 'visual-line-mode)  ; Soft wrap lines
(add-hook 'text-mode-hook 'flyspell-mode)     ; Spell-check in text
(add-hook 'python-mode-hook 'flyspell-prog-mode)  ; Spell-check strings/comments

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
;; Load Custom Files and Modes
;; --------------------------------------

;; Load blog.el (custom file)
(load "blog")  ; Loads ~/.emacs.d/site/apo/blog.el

;; Enable other modes
(require 'hy-mode)     ; Hy-lang support
(require 'pyenv-mode)  ; Python virtualenv integration

;; Enable better-defaults
(require 'better-defaults)

;; --------------------------------------
;; Final Settings
;; --------------------------------------

(put 'downcase-region 'disabled nil)  ; Enable case commands
(put 'upcase-region 'disabled nil)

;; --------------------------------------
;; End of Configuration
;; --------------------------------------
