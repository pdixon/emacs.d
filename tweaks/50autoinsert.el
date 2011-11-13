;;; 50autoinsert.el --- Setup for autoinsert

;; Copyright (C) 2010  Phillip Dixon

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

(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (concat dotfiles-dir "mytemplates/"))
(setq auto-insert-query nil)

(defun pd-parse-yas-from-file (file)
  (with-temp-buffer
    (when (file-readable-p file)
      (insert-file-contents file nil nil nil t)
      (yas/parse-template file))))

(defun pd-expand-yas-template-from-file (file)
  "expand the snippet read from FILE."
  (yas/expand-snippet
   (second (pd-parse-yas-from-file file))))

(defun pd-auto-insert-template (template)
  (pd-expand-yas-template-from-file
   (concat auto-insert-directory template)))

(define-auto-insert "\.markdown"
  '(lambda () (pd-auto-insert-template "post.markdown")))

(provide '50autoinsert)
;;; 50autoinsert.el ends here
