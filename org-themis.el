
;;; org-themis.el --- Experimental project management extenions for org-mode

;; Copyright (C) 2015 Zachary Elliott
;;
;; Authors: Zachary Elliott <contact@zell.io>
;; URL: http://github.com/zellio/org-themis
;; Version: 0.4.0
;; Keywords: org-mode, elisp, project

;; This file is not part of GNU Emacs.

;;; Commentary:

;;

;;; License:

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Code:

(defconst org-themis-version "0.4.0"
  "`org-themis' version")

(defgroup org-themis nil
  "org-themis application group"
  :group 'applications
  :prefix "org-themis-")

(defcustom org-themis-project-root "~/projects"
  "Parent directory for `org-themis' projects"
  :type 'string
  :group 'org-themis)

(defcustom org-themis-project-alist-file
  (expand-file-name ".org-themis-project-alist" org-themis-project-root)
  "`org-themis' project alist file"
  :type 'string
  :group 'org-themis)

(defun org-themis--load-project-alist ()
  ""
  (if (file-exists-p org-themis-project-alist-file)
      (with-temp-buffer
        (insert-file-contents org-themis-project-alist-file)
        (read (current-buffer)))
    '()))

(defun org-themis--save-project-alist ()
  ""
  (with-temp-buffer
    (insert
     ";; Header"
     (prin1-to-string org-themis-project-alist))
    (write-file org-themis-project-alist-file nil)))

(defvar org-themis-project-alist (org-themis--load-project-alist)
  "")

(defun org-themis--list-projects ()
  ""
  (mapcar #'car org-themis-project-alist))

(defun org-themis--add-project (name root scratch journal)
  ""
  (push
   (cons name
         (list
          (cons 'name (symbol-name name))
          (cons 'root root)
          (cons 'scratch scratch)
          (cons 'journal journal)))
   org-themis-project-alist))

(defun org-themis-add-project-interactive (name)
  ""
  (interactive "SProject Name: ")
  (if (null (assoc name org-themis-project-alist))
      (org-themis--add-project
       name
       (expand-file-name (symbol-name name) org-themis-project-root)
       "scratch.org"
       "journal.org")
    (error "Name %s already in use" name)))

(defun org-themis--remove-project (name)
  ""
  (setq
   org-themis-project-alist
   (remove-if
    (lambda (x) (eql name (car x)))
    org-themis-project-alist)))

(defun org-themis-remove-project-interactive (name)
  ""
  (interactive "SProject Name: ")
  (org-themis--remove-project name))

(defvar org-themis-project 'nil
  "")

(defvar org-themis-project-data 'nil
  "")

(defun org-themis-set-project (name data)
  ""
  (setq org-themis-project name)
  (setq org-themis-project-data data))

(defun org-themis-set-project-interactive (name)
  ""
  (interactive "SProject Name: ")
  (let ((data (assoc name org-themis-project-alist)))
    (if data
        (progn
          (org-themis-set-project name data)
          (messsage "Project set to %s" name))
      (error "No project named %s found" name))))

;;; org-themis.el ends here
