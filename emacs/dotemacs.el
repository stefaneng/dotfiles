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

;; Start eshell on startup
(add-hook 'emacs-startup-hook #'(lambda ()
				  (let ((default-directory (getenv "HOME")))
				    (command-execute 'eshell)
				    (bury-buffer))))

;; Open up the dotfiles on startup since I always add things to it
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (find-file "~/dotfiles/emacs/dotemacs.el")
	      (bury-buffer)))

;; js2-mode
(ensure-installed-package 'js2-mode
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  ;;(add-to-list 'ac-js2-external-libraries "path/to/lib/library.js")
  ;;(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
)

;; web-mode
(ensure-installed-package 'web-mode
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
)

;; Hide startup screen
(setq inhibit-startup-message t)

;; Inconsolata font
;; TODO: Set fallback fonts?
(set-face-attribute 'default nil :family "Inconsolata" :height 135)

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

;; Add F11 for changing from perl to prolog
(add-hook 'perl-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f11>") 'prolog-mode)))
;; Don't need idl shit, prolog yoooooooo
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))

;; Markdown mode
(ensure-installed-package 'markdown-mode
  ;;(autoload 'markdown-mode "markdown-mode"
  ;;   "Major mode for editing Markdown files" t)
  (require 'markdown-mode)
  ;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  ;; Github style markdown for readme's
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  ;; Change to whatever markdown parser you like
  (setq markdown-command "markdown_py")
)

;; Magit
(ensure-installed-package 'magit
  (global-set-key (kbd "C-x g")
		  'magit-status)
)

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

;; Haskell mode
(ensure-installed-package 'haskell-mode
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
)

;; C Stuff
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'c-mode-hook
	  #'(lambda ()
	      (local-set-key (kbd "<f11>") 'c-man)))

;(package-refresh-contents)

;;; dotemacs.el ends here
