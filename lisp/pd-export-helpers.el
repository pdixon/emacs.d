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

(require 'ox)
(require 'ox-html)

(defun pd-export-html-string (filename)
  ""
  (with-temp-buffer
    (insert-file-contents filename)
    (org-export-as 'html nil nil t nil)))

(provide 'pd-export-helpers)
;;; pd-export-helpers.el ends here
