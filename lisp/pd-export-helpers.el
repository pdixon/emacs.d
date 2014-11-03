;;; pd-export-helpers.el --- Helpers for working with ox  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: 

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

;; 

;;; Code:

(require 'f)
(require 'ox)
(require 'ox-html)
(require 'vc-git)

(defun pd-files-in-dir (extension dir)
  ""
  (f--files dir (equal (f-ext it) extension)))

(defun pd-file-env (file)
  ""
  (let ((git-date (pd-git-timestamp file))
        (env (org-babel-with-temp-filebuffer file (org-export-get-environment))))
    (plist-put env :file file)
    (plist-put env :git-date git-date)))

(defun pd-git-timestamp (file)
  ""
  (date-to-time
   (vc-git--run-command-string file "log" "-i" "--format=%ci")))

(defun pd-org-timestamp< (a b)
  ""
  (org-time< (org-element-property :raw-value (car a))
             (org-element-property :raw-value (car b))))

(defun pd-export-html-string (filename)
  ""
  (with-temp-buffer
    (insert-file-contents filename)
    (org-export-as 'html nil nil t nil)))

(provide 'pd-export-helpers)
;;; pd-export-helpers.el ends here
