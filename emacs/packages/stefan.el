;;; manage-packages.el --- provides some simple package functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Stefan Eng <stefaneng13@gmail.com>
;; Version: 0.1
;; Keywords: package
;; Created: 2013-11-10
;; URL: https://github.com/stefaneng/dotfiles

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides some useful package functions to manage packages.

;;; Usage:

;;; Bugs: https://github.com/stefaneng/dotfiles/issues

;;; Code:

(require 'package)

;;;###autoload
(defmacro ensure-installed-package (package &rest body)
  "Ensures PACKAGE is installed and then performs body. Place package configurations in BODY"
  (declare (indent 1))    
  `(progn 
     (when (not (package-installed-p ,package))
       (package-install ,package))
     ,@body))

(defun c-man ()
  "Gets the man(5) page for the current word when in C mode."
  (interactive) 
  (man 
   (concat "3" (current-word))))

(provide 'stefan)
;;; stefan.el ends here




