;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

;;; Code:
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/")
  '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'load-path "~/.emacs.d/site/neotree")
(require 'neotree)

(add-to-list 'load-path "~/.emacs.d/site/autopair")
(require 'autopair)
(autopair-global-mode)

(add-to-list 'load-path "~/.emacs.d/site/jinja2-mode")
(require 'jinja2-mode)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(require 'evil)
;; (evil-mode 1)
(global-undo-tree-mode 0)
;; BASIC CUSTOMIZATION
;; --------------------------------------

;; (setq inhibit-startup-message t) ;; hide the startup message
(delete-selection-mode nil)
(load-theme 'wombat t) ;; load material theme




(require 'color)
(require 'hl-line)

;; (set-face-foreground 'highlight nil)
;; (set-face-foreground 'hl-line nil)

(defun set-hl-line-color-based-on-theme ()
  "Sets the hl-line face to have no foregorund and a background
    that is 10% darker than the default face's background."
  (set-face-attribute 'hl-line nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 10)))


(set-face-attribute 'hl-line nil
                    :inherit nil
                    :background (face-background 'highlight))
(add-hook 'global-hl-line-mode-hook 'set-hl-line-color-based-on-theme)
;; (set-face-background 'hl-line "light yellow")

(global-hl-line-mode 1)
(global-hl-line-mode 0)
(global-hl-line-mode 1)

(global-linum-mode t) ;; enable line numbers globally
(setq column-number-mode t)

(elpy-enable)

;; (add-to-list 'load-path "~/.emacs.d/site/flymake-mypy")
;; (eval-after-load 'flymake '(require 'flymake-mypy))


;; (when (load "flymake" t)
;;   (defun flymake-pylint-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "epylint" (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
               ;; '("\\.py\\'" flymake-pylint-init)))

(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)
(global-set-key (kbd "M-3") 'windmove-up)
(global-set-key (kbd "M-4") 'windmove-down)
(global-set-key (kbd "C-c o") 'occur)
;; (set-face-underline-p 1)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)


;; Magit configuration
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x M-b") 'magit-blame)


;; Elpy configuration
(global-set-key (kbd "M-*") 'pop-tag-mark)

(add-hook 'after-init-hook #'global-flycheck-mode)
;; (add-to-list 'flycheck-checkers 'python-pylint)

;; (set-face-background 'hl-line "#3e4446")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-project-root-finder-functions
   (quote
    (elpy-project-find-projectile-root elpy-project-find-python-root elpy-project-find-git-root elpy-project-find-hg-root elpy-project-find-svn-root)))
 '(elpy-rpc-backend "rope")
 '(elpy-syntax-check-command "flake8")
 '(elpy-test-pytest-runner-command (quote ("py.test" "-s")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(package-selected-packages
   (quote
    (flycheck-pylint flycheck-pyflakes flycheck-mypy yaml-mode magit material-theme json-mode flycheck evil-text-object-python elpy better-defaults)))
 '(python-check-command "flake8")
 '(python-indent-trigger-commands
   (quote
    (indent-for-tab-command yas-expand yas/expand newline)))
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "--simple-prompt"))


;; (setq python-shell-interpreter "ipython"
;;     python-shell-interpreter-args "--simple-prompt -i")

(setq python-shell-interpreter "ipython")
;; (setq elpy-rpc-backend "jedi")



(add-hook 'python-mode-hook 'evil-text-object-python-add-bindings)

(add-hook 'text-mode-hook 'longlines-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :underline nil)))))
(put 'upcase-region 'disabled nil)
