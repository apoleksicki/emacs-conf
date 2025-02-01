;; init.el --- Emacs configuration

;; Package Management
;; --------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Install required packages
(defvar my-packages
  '(better-defaults material-theme neotree autopair elpy traad pyenv-mode blog hy-mode typewritter evil flycheck flycheck-pycheckers))
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Basic Customization
;; --------------------------------------

(set-face-attribute 'default nil :height 150)
(global-hl-line-mode 1)
(delete-selection-mode nil)
(load custom-file)

;; Keybindings
;; --------------------------------------

(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)
(global-set-key (kbd "M-3") 'windmove-up)
(global-set-key (kbd "M-4") 'windmove-down)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

;; Magit configuration
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x M-b") 'magit-blame)

;; Mode Configuration
;; --------------------------------------

;; Neotree
(require 'neotree)
(evil-define-key 'normal neotree-mode-map
  (kbd "TAB") 'neotree-enter
  (kbd "SPC") 'neotree-quick-look
  (kbd "q") 'neotree-hide
  (kbd "RET") 'neotree-enter
  (kbd "g") 'neotree-refresh
  (kbd "n") 'neotree-next-line
  (kbd "p") 'neotree-previous-line
  (kbd "A") 'neotree-stretch-toggle
  (kbd "H") 'neotree-hidden-file-toggle)

;; Evil Mode
(require 'evil)
(evil-mode 1)

;; Elpy (Python Development)
(require 'elpy)
(elpy-enable)
(global-set-key (kbd "M-*") 'pop-tag-mark)

;; Flycheck
(require 'flycheck)
(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)
(require 'flycheck-pycheckers)
(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; Text and Programming Modes
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)

;; Custom Variables and Faces
;; --------------------------------------

(custom-set-variables
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes '(deeper-blue))
 '(custom-safe-themes '("b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" default))
 '(fci-rule-color "#ECEFF1")
 '(hl-sexp-background-color "#efebe9")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; Custom faces
 )

;; Helper Functions
;; --------------------------------------

(defun inner-open-project (directory)
  "Open a project in the specified DIRECTORY."
  (interactive "DOpen project: ")
  (pyvenv-activate directory)
  (let ((project-name (car (last (split-string directory "/")))))
    (find-file (format "~/dev/%s/setup.py" project-name))))

(defun open-project ()
  "Open a project interactively."
  (interactive)
  (let ((default-directory "~/.pyenv/versions/"))
    (call-interactively 'inner-open-project)))

;; Load Paths
;; --------------------------------------

(add-to-list 'load-path "~/.emacs.d/elpa/autopair-20160304.1237")
(add-to-list 'load-path "~/.emacs.d/site/better-defaults")
(add-to-list 'load-path "~/.emacs.d/site/apo")
(add-to-list 'load-path "~/.emacs.d/site/apo/typewritter-mode")
(add-to-list 'load-path "~/.emacs.d/site/hy-mode")
(add-to-list 'load-path "~/.emacs.d/site/flymake-mypy")

;; Enable Autopair
(require 'autopair)
(autopair-global-mode)

;; Enable Better Defaults
(require 'better-defaults)

;; Enable Other Modes
(require 'traad)
(require 'pyenv-mode)
(require 'blog)
(require 'hy-mode)
(require 'typewritter)

;; Final Setup
;; --------------------------------------

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
