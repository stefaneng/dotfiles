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

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Hide startup screen
(setq inhibit-startup-message t)

;; Inconsolata font
;; TODO: Set fallback fonts?
(set-face-attribute 'default nil :family "Inconsolata" :height 120)

;; From http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/Backups/emacs"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; Remove the scroll bar, menu bar, and tool bar
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Add F11 for changing between perl and prolog
;(eval-after-load 'perl
(define-key perl-mode-map 
  (kbd "<f11>") 'prolog-mode)

(define-key prolog-mode-map
  (kbd "<f11>") 'perl-mode)

;; Tabs are evil
(setq indent-tabs-mode nil)

;; Line number mode
(global-linum-mode 1)

;; Tango-dark theme for now
(load-theme 'tango-dark)

;; Haskell mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; C Stuff
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; Package management stuff
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives 
;	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; Add my package folder
(add-to-list 'package-archives
	     '("local" . "/home/stefan/dotfiles/emacs/packages/") t)

(package-initialize)


;(package-refresh-contents)

;;; dotemacs.el ends here
