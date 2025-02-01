;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

;;; Code:
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/")
  '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(require 'neotree)
(set-face-attribute 'default nil :height 150)

(add-to-list 'load-path "~/.emacs.d/elpa/autopair-20160304.1237")
(add-to-list 'load-path "~/.emacs.d/site/better-defaults")
(add-to-list 'load-path "~/.emacs.d/site/apo")
(add-to-list 'load-path "~/.emacs.d/site/apo/typewritter-mode")
(add-to-list 'load-path "~/.emacs.d/site/hy-mode")
(require 'autopair)
(require 'better-defaults)
(require 'elpy)
(require 'traad)
(require 'pyenv-mode)
(require 'blog)
(require 'hy-mode)
(require 'typewritter)
(autopair-global-mode)

(load custom-file)
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

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)
(global-set-key (kbd "M-3") 'windmove-up)
(global-set-key (kbd "M-4") 'windmove-down)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

(elpy-enable)

;; Magit configuration
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-x M-b") 'magit-blame)

(require 'evil)
(evil-mode 1)

;; evil config
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; (global-undo-tree-mode 0)
;; ;; BASIC CUSTOMIZATION
;; ;; --------------------------------------

;; Elpy configuration
(global-set-key (kbd "M-*") 'pop-tag-mark)

;; (add-to-list 'flycheck-checkers 'python-pylint)

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
 '(fci-rule-color "#ECEFF1")
 '(flycheck-pycheckers-checkers (quote (pylint pep8 flake8 mypy3)))
 '(hl-sexp-background-color "#efebe9")
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
;;     python-shell-interpreter-args "--simple-prompt -i")



(delete-selection-mode nil)





(add-to-list 'load-path "~/.emacs.d/site/flymake-mypy")
(eval-after-load 'flymake '(require 'flymake-mypy))



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



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; flycheck fix
(require 'flycheck)

(add-to-list 'flycheck-disabled-checkers 'python-flake8)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)


(require 'flycheck-pycheckers)
(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))




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

