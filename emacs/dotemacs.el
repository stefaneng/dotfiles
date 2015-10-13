;;(menu-bar-mode -1)

(setq package-list
      '(material-theme magit))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Use-package to ensure packages are installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)


; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; Theme
(use-package material-theme
  :ensure t
  :init (load-theme 'material t))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Company Mode
(use-package company
  :ensure t
  :config (global-company-mode))

(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 150
                    :weight 'normal
                    :width 'normal)

(global-linum-mode 1)
;; Keep line numbers small
(set-face-attribute 'linum nil :height 100)

;; Settings
(setq use-dialog-box nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq-default indent-tabs-mode nil)

(define-key global-map (kbd "RET") 'newline-and-indent)

;; Keep a backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
