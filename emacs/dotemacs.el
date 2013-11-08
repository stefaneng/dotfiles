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

;; Tango-dark theme for now
(load-theme 'tango-dark)

;; Use this monokai theme from the repos
; (load-theme 'monokai t)

;; Package management stuff

;; Declare what packages we want installed
(require 'package)

;; Marmalade package archive
;; http://marmalade-repo.org/

(add-to-list 'package-archives 
    '("marmalade" .
          "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; dotemacs.el ends here
