;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

;;; Code:
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/")
  '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;;;(require 'neotree)

(add-to-list 'load-path "~/.emacs.d/elpa/autopair-20160304.1237")
(add-to-list 'load-path "~/.emacs.d/site/better-defaults")
(require 'autopair)
(require 'better-defaults)
(autopair-global-mode)


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

(global-hl-line-mode 1)
;; (global-linum-mode t)
;; (global-undo-tree-mode 0)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)
(global-set-key (kbd "M-3") 'windmove-up)
(global-set-key (kbd "M-4") 'windmove-down)
(global-set-key (kbd "C-c o") 'occur)
;; (set-face-underline-p 1)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

(elpy-enable)

;; Magit configuration
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x M-b") 'magit-blame)

;; (require 'evil)
;; ;; (evil-mode 1)
;; (global-undo-tree-mode 0)
;; ;; BASIC CUSTOMIZATION
;; ;; --------------------------------------

;; Elpy configuration
(global-set-key (kbd "M-*") 'pop-tag-mark)

;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (add-to-list 'flycheck-checkers 'python-pylint)

;; (set-face-background 'hl-line "#3e4446")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-project-root-finder-functions
   (quote
    (elpy-project-find-projectile-root elpy-project-find-python-root elpy-project-find-git-root elpy-project-find-hg-root elpy-project-find-svn-root)))
 '(elpy-rpc-backend "rope")
 '(elpy-syntax-check-command "mypy")
 '(elpy-test-pytest-runner-command (quote ("py.test" "-s")))
 '(elpy-test-runner (quote elpy-test-pytest-runner))
 '(fci-rule-color "#ECEFF1")
 '(flycheck-pycheckers-checkers (quote (pylint pep8 flake8 mypy3)))
 '(hl-sexp-background-color "#efebe9")
 '(package-selected-packages
   (quote
    (darkroom importmagic traad flycheck-pycheckers visual-fill-column flycheck-pylint flycheck-pyflakes yaml-mode magit material-theme json-mode flycheck evil-text-object-python elpy better-defaults)))
 '(python-check-command "flake8")
 '(python-indent-trigger-commands
   (quote
    (indent-for-tab-command yas-expand yas/expand newline)))
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "--simple-prompt")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
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
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil))

(add-hook 'python-mode-hook 'flyspell-prog-mode)
;; (setq python-shell-interpreter "ipython"
;;     python-shell-interpreter-args "--simple-prompt -i")

(setq python-shell-interpreter "ipython")
;; (setq elpy-rpc-backend "jedi")


;; ;; (setq inhibit-startup-message t) ;; hide the startup message
(delete-selection-mode nil)
;; (load-theme 'wombat t) ;; load material theme




;; (require 'color)
;; (require 'hl-line)

;; ;; (set-face-foreground 'highlight nil)
;; ;; (set-face-foreground 'hl-line nil)

;; (defun set-hl-line-color-based-on-theme ()
;;   "Sets the hl-line face to have no foregorund and a background
;;     that is 10% darker than the default face's background."
;;   (set-face-attribute 'hl-line nil
;;                       :foreground nil
;;                       :background (color-darken-name (face-background 'default) 10)))


;; (set-face-attribute 'hl-line nil
;;                     :inherit nil
;;                     :background (face-background 'highlight))
;; (add-hook 'global-hl-line-mode-hook 'set-hl-line-color-based-on-theme)
;; ;; (set-face-background 'hl-line "light yellow")

;; (global-hl-line-mode 1)
;; (global-hl-line-mode 0)
;; (global-hl-line-mode 1)

;; (global-linum-mode t) ;; enable line numbers globally
;; (setq column-number-mode t)

;; (elpy-enable)

(add-to-list 'load-path "~/.emacs.d/site/flymake-mypy")
(eval-after-load 'flymake '(require 'flymake-mypy))


;; ;; (when (load "flymake" t)
;; ;;   (defun flymake-pylint-init ()
;; ;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; ;;                        'flymake-create-temp-inplace))
;; ;;            (local-file (file-relative-name
;; ;;                         temp-file
;; ;;                         (file-name-directory buffer-file-name))))
;; ;;       (list "epylint" (list local-file))))
;; ;;   (add-to-list 'flymake-allowed-file-name-masks
;;                ;; '("\\.py\\'" flymake-pylint-init)))

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


;; ;; elpy configuration
;; (global-set-key (kbd "M-*") 'pop-tag-mark)

;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; ;; (add-to-list 'flycheck-checkers 'python-pylint)

;; ;; (set-face-background 'hl-line "#3e4446")
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(elpy-modules
;;    (quote
;;     (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
;;  '(elpy-project-root-finder-functions
;;    (quote
;;     (elpy-project-find-projectile-root elpy-project-find-python-root elpy-project-find-git-root elpy-project-find-hg-root elpy-project-find-svn-root)))
;;  '(elpy-rpc-backend "rope")
;;  '(elpy-syntax-check-command "flake8")
;;  '(elpy-test-pytest-runner-command (quote ("py.test" "-s")))
;;  '(elpy-test-runner (quote elpy-test-pytest-runner))
;;  '(package-selected-packages
;;    (quote
;;     (flycheck-pylint flycheck-pyflakes flycheck-mypy yaml-mode magit material-theme json-mode flycheck evil-text-object-python elpy better-defaults)))
;;  '(python-check-command "flake8")
;;  '(python-indent-trigger-commands
;;    (quote
;;     (indent-for-tab-command yas-expand yas/expand newline)))
;;  '(python-shell-interpreter "ipython")
;;  '(python-shell-interpreter-args "--simple-prompt"))


;; ;; (setq python-shell-interpreter "ipython"
;; ;;     python-shell-interpreter-args "--simple-prompt -i")

;; (setq python-shell-interpreter "ipython")
;; ;; (setq elpy-rpc-backend "jedi")



;; (add-hook 'python-mode-hook 'evil-text-object-python-add-bindings)

;; (add-hook 'text-mode-hook 'longlines-mode)
;; (add-hook 'text-mode-hook 'flyspell-mode)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(hl-line ((t (:inherit highlight :underline nil)))))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector
;;    [default bold shadow italic underline bold bold-italic bold])
;;  '(ansi-color-names-vector
;;    (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
;;  '(custom-safe-themes
;;    (quote
;;     ("b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" default)))
;;  '(fci-rule-color "#37474f")
;;  '(hl-sexp-background-color "#1c1f26")
;;  '(vc-annotate-background nil)
;;  '(vc-annotate-color-map
;;    (quote
;;     ((20 . "#f36c60")
;;      (40 . "#ff9800")
;;      (60 . "#fff59d")
;;      (80 . "#8bc34a")
;;      (100 . "#81d4fa")
;;      (120 . "#4dd0e1")
;;      (140 . "#b39ddb")
;;      (160 . "#f36c60")
;;      (180 . "#ff9800")
;;      (200 . "#fff59d")
;;      (220 . "#8bc34a")
;;      (240 . "#81d4fa")
;;      (260 . "#4dd0e1")
;;      (280 . "#b39ddb")
;;      (300 . "#f36c60")
;;      (320 . "#ff9800")
;;      (340 . "#fff59d")
;;      (360 . "#8bc34a"))))
;;  '(vc-annotate-very-old-color nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
 ;; )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; flycheck fix
(require 'flycheck)
;; (require 'flycheck-mypy)
;; (defun fix-flake8 (errors)
;;           (let ((errors (flycheck-sanitize-errors errors)))
;;             (seq-do #'flycheck-flake8-fix-error-level errors)
;;             errors))

;;         (flycheck-define-checker python-flake8-chain
;;           "A Python syntax and style checker using flake8"
;;           :command ("flake8"
;;                      "--format=default"
;;                      (config-file "--config" flycheck-flake8rc)
;;                      (option "--max-complexity" flycheck-flake8-maximum-complexity nil
;;                        flycheck-option-int)
;;                      (option "--max-line-length" flycheck-flake8-maximum-line-length nil
;;                        flycheck-option-int)
;;                      "-")
;;           :standard-input t
;;           :error-filter fix-flake8
;;           :error-patterns
;;           ((warning line-start
;;              "stdin:" line ":" (optional column ":") " "
;;              (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;              (message (one-or-more not-newline))
;;              line-end))
;;           :next-checkers ((t . python-pylint))
;;           :modes python-mode)

        ;; replace flake8 with new chaining one from above
;; (setq flycheck-checkers (cons 'python-flake8-chain (delq 'python-flake8 flycheck-checkers)))

(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)


(require 'flycheck-pycheckers)
(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))



;; (require 'flycheck-mypy)
;; (add-hook 'python-mode-hook 'flycheck-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun inner-open-project (directory)
  (interactive "DOpen project: ")
  (pyvenv-activate directory)
  (message directory)
  (let ((project-name (nth 0 (last (split-string directory "/")))))
    (message project-name)
    (message (format "~/dev/%s/setup.py" project-name))
    (find-file (format "~/dev/%s/setup.py" project-name))))

(defun open-project ()
  (interactive)
  (let ((default-directory "~/.pyenv/versions/"))
    (call-interactively 'inner-open-project)))

