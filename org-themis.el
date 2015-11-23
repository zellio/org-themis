
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

(require 'cl)

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
        (or (read (current-buffer)) '()))
    '()))

(defun org-themis--save-project-alist ()
  ""
  (with-temp-buffer
    (insert
     ";; -*- mode: lisp; coding: utf-8 -*-\n"
     ";; Header\n"
     "\n"
     "(")
    (dolist (project org-themis-project-alist)
      (newline)
      (insert " (" (prin1-to-string (car project)) "\n")
      (dolist (arg (cdr project))
        (insert "  " (prin1-to-string arg) "\n"))
      (delete-char -1)
      (insert ")"))
    (insert ")")
    (goto-char (point-min))
    (forward-line 3)
    (forward-char 1)
    (delete-char 2)
    (write-file org-themis-project-alist-file nil)))

(defvar org-themis-project-alist (org-themis--load-project-alist)
  "")

(defun org-themis--list-projects ()
  ""
  (mapcar #'car org-themis-project-alist))

(defun org-themis--completing-read-project-selector ()
  (list
   (intern
    (completing-read "Project name: " (org-themis--list-projects)))))

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
  (interactive (org-themis--completing-read-project-selector))
  (org-themis--remove-project name))

(defvar org-themis-project nil
  "")

(defvar org-themis-project-data nil
  "")

(defun org-themis-set-project (name data)
  ""
  (setq org-themis-project name)
  (setq org-themis-project-data data))

(defun org-themis-set-project-interactive (name)
  ""
  (interactive (org-themis--completing-read-project-selector))
  (let ((data (assoc name org-themis-project-alist)))
    (if data
        (org-themis-set-project name data)
      (error "No project named %s found" name))))

(dolist (tag '(name root scratch journal))
  (eval
   `(defun ,(intern (format "org-themis--project-%s" tag)) ()
      ""
      (cdr (assoc (quote ,tag) (cdr org-themis-project-data))))
   ))

(defun org-themis-find-project-file (&optional file)
  ""
  (interactive)
  (let ((default-directory (format "%s/" (org-themis--project-root))))
    (if file
        (find-file (expand-file-name file default-directory))
      (command-execute 'find-file))))

(defun org-themis-find-project-root ()
  ""
  (interactive)
  (org-themis-find-project-file "."))

(defun org-themis-find-project-journal ()
  ""
  (interactive)
  (org-themis-find-project-file (org-themis--project-journal)))

(defun org-themis-find-project-scratch ()
  ""
  (interactive)
  (org-themis-find-project-file (org-themis--project-scratch)))

(defun org-themis--update-minor-mode-lighter ()
  ""
  (setcdr
   (assoc 'org-themis-mode minor-mode-alist)
   (list
    (if org-themis-project
        (format " ot:%s" org-themis-project)
      " ot"))))

(defvar org-themis-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c /") 'org-themis-find-project-file)
    (define-key map (kbd "C-c !")
      #'(lambda ()
          (interactive)
          (command-execute 'org-themis-set-project-interactive)
          (org-themis--update-minor-mode-lighter)))
    (define-key map (kbd "C-c +")
      #'(lambda ()
          (interactive)
          (command-execute 'org-themis-add-project-interactive)
          (org-themis--save-project-alist)))
    (define-key map (kbd "C-c -")
      #'(lambda ()
          (interactive)
          (command-execute 'org-themis-remove-project-interactive)
          (org-themis--save-project-alist)))
    map)
  "Keymap for org-themis minor mode.")

(define-minor-mode org-themis-mode
  "Experimental project management extenions for org-mode"
  :lighter " ot"
  :keymap org-themis-mode-map
  :group 'org-themis)

;;; org-themis.el ends here
