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

(package-install 'use-package)
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

(use-package haskell-mode
  :ensure t
  :bind ("C-c C-l" . haskell-process-load-or-reload)
  :init (progn
          (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
          (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
          (setq haskell-process-type 'stack-ghci)
          (setq haskell-process-path-ghci "stack")
          (setq haskell-process-args-ghci "ghci")
          ))

;; Company Mode
(use-package company
  :ensure t
  :config (global-company-mode))

;; Ghci
(use-package company-ghci
  :ensure t
  :init (progn
          (require 'company-ghci)
          (push 'company-ghci company-backends)
          (add-hook 'haskell-mode-hook 'company-mode)))

;; js2-mode
(use-package js2-mode
  :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
            (setq js2-basic-offset 2)))

;; Web-mode
(use-package web-mode
  :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))))

(use-package ess
  :ensure t)

;; Osx specific stuff
(if (eq system-type 'darwin)
    (progn
      (use-package osx-dictionary
        :ensure t
        :bind ("C-c d" . osx-dictionary-search-pointer))
      ;; Font
      (set-face-attribute 'default nil
                          :family "Consolas"
                          :height 150
                          :weight 'normal
                          :width 'normal)
      )
)

;; Helm mode
;; Config based on http://pages.sachachua.com/.emacs.d/Sacha.html
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
;          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
;         ("C-x c s" . helm-swoop)
;         ("C-x c y" . helm-yas-complete)
;         ("C-x c Y" . helm-yas-create-snippet-on-region)
;         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

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
(blink-cursor-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)

;; Custom keymaps
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Keep a backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Whitespace mode
(global-whitespace-mode 1)

;; Newline at end of file
(setq require-final-newline t)
(setq next-line-add-newlines nil)

(add-hook 'before-save-hook 'whitespace-cleanup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(js-indent-level 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
