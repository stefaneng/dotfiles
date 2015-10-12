;;(menu-bar-mode -1)

(setq package-list
      '(material-theme magit))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Theme
(load-theme 'material t)

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

(setq auto-save-default nil)
(setq make-backup-files nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Magit status
(global-set-key (kbd "C-x g")
		'magit-status)
