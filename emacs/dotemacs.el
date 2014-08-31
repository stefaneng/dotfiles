;;; dotemacs.el --- My init file

;; Copyright (C) 2013 Stefan Eng

;; Author:  Stefan Eng <stefaneng13@gmail.com>
;; URL: https://github.com/stefaneng/dotfiles
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; Add my packages to load path
;(add-to-list 'load-path "/home/stefan/dotfiles/emacs/packages")

;; TODO?
;; - js3.el mode https://github.com/thomblake/js3-mode
;; - jshint mode - slightly patched version of original jshint which uses node and loads jshint config from home -   https://github.com/jeffbski/jshint-mode
;; - json-pretty-print - https://github.com/thorstadt/json-pretty-print.el
;; - ido - great for quick access to buffers and files - http://ubuntu2.wordpress.com/2008/01/02/emacs-tip-1-ido-mode/
;; - markdown-mode - http://emacswiki.org/emacs/MarkdownMode
;; - uniquify - provides nice unique short buffer names - http://trey-jackson.blogspot.com/2008/01/emacs-tip-11-uniquify.html
;; - anything mode - http://www.emacswiki.org/Anything


;; Package management stuff
(require 'package)

;; My functions
(add-to-list 'load-path "~/dotfiles/emacs/packages")
(require 'stefan)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Add my package folder
(add-to-list 'package-archives
	     '("local" . "/home/stefan/dotfiles/emacs/packages/") t)

(package-initialize)
;(package-refresh-contents)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")

(setq my-packages
      '(el-get
	ghc-mod
	simple-httpd
	flycheck
	js2-mode
	exec-path-from-shell
	web-mode
	markdown-mode
	git-gutter-fringe
	ess
	smartparens
	auto-complete
	ac-ghc-mod
	coffee-mode
	magit
	org-mode
	cdlatex-mode))

(el-get 'sync my-packages)

;; Start eshell on startup
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (let ((default-directory (getenv "HOME")))
		(command-execute 'eshell)
		(bury-buffer))))

;; Open up the dotfiles on startup since I always add things to it
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (find-file "~/dotfiles/emacs/dotemacs.el")
	      (bury-buffer)))

;; On save, remove whitespace at end of lines
(add-hook 'before-save-hook
	  #'(lambda ()
	      (delete-trailing-whitespace)))

;; Hide startup screen
(setq inhibit-startup-message t)

;; Stop the noise
(setq ring-bell-function 'ignore)

;; Inconsolata font
;; TODO: Set fallback fonts?
;(set-face-attribute 'default nil :family "Roboto" :height 140)

;; From http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t             ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/Backups/emacs"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)              ; use versioned backups

;; Remove the scroll bar, menu bar, and tool bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Unique buffer names by adding directory name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Add F11 for changing from perl to prolog
(add-hook 'perl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f11>") 'prolog-mode)))
;; Don't need idl shit, prolog yoooooooo
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;; Python
(setq python-shell-interpreter "ipython")

;; Emacs Lisp
(dolist (hook
	 '(emacs-lisp-mode-hook
	   lisp-interaction-mode-hook
	   lisp-interaction-mode-hook))
  (add-hook hook #'(lambda ()
		     (turn-on-eldoc-mode)
		     (enable-paredit-mode))))

;; Linum
;(setq linum-format line-number-to-spaces)

;; Helm stuff
(ensure-installed-package 'helm
  (helm-mode 1)
  (add-hook 'eshell-mode-hook
	    #'(lambda ()
		(define-key eshell-mode-map
        	  (kbd "<tab>") 'helm-esh-pcomplete)))
)

;; Tabs are evil
(setq indent-tabs-mode nil)

;; Line number mode
(global-linum-mode 1)
;; Column numbers
(column-number-mode 1)

;; Tango-dark theme for now
;;(load-theme 'tango-dark)
;; Sublime text like theme
;(ensure-installed-package 'monokai-theme
;  (load-theme 'monokai t)
;)
(ensure-installed-package 'zenburn-theme
  (load-theme 'zenburn t)
)

;; Clojure
(ensure-installed-package 'clojure-mode
  ;; Regular clojure stuff
  (add-hook 'clojure-mode-hook
	    #'(lambda ()
		(enable-paredit-mode))))
(ensure-installed-package 'cider
  ;; REPL Mode hooks
  (add-hook 'cider-repl-mode-hook
	    #'(lambda ()
		(paredit-mode)))
  ;; Cider mode hooks
  (add-hook 'cider-mode-hook
	    #'(lambda ()
		(cider-turn-on-eldoc-mode)
		(enable-paredit-mode)))
  (setq nrepl-hide-special-buffers t))

;; Haskell mode
(ensure-installed-package 'haskell-mode
  (add-hook 'haskell-mode-hook
            (lambda ()
		(flycheck-mode -1)
		;(autoload 'ghc-init "ghc" nil t)
		;(autoload 'ghc-debug "ghc" nil t)
		(ghc-init)
                (turn-on-haskell-indent)
;                (turn-on-haskell-doc-mode)
;                (flycheck-select-checker 'haskell-hlint)
		))

)

;; C Stuff
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c-mode-hook
	  #'(lambda ()
	      (local-set-key (kbd "<f11>") 'c-man)))

;; Flyspell
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; LaTeX
(ensure-installed-package 'auctex
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

  (setq TeX-PDF-mode t)
)

;; No graphical popups... it freezes osx
(setq use-dialog-box nil)

;; OSX Specific changes
(if (eq system-type 'darwin)
    (progn
      (menu-bar-mode)
      (set-face-attribute 'default nil :family "Consolas" :height 155)
      )
  ;; No else
)

;(package-refresh-contents)

;;; dotemacs.el ends here
