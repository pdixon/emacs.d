;;; sql-linx.el --- SQLi specialisations for LiNX -*- lexical-binding: t -*-

;; Copyright (C) since 2019 Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords:

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'sql)

(defgroup sql-linx nil
  ""
  :group 'SQL
  :prefix "sql-linx-")

(defcustom sql-linx-program "linx-shell"
  "Command to run a linx interpreter."
  :type 'file
  :group 'sql-linx)

(defcustom sql-linx-options '()
  "List of options for `sql-linx-program'."
  :type '(repeat string)
  :group 'sql-linx)

;;;###autoload
(defun sql-linx (&optional buffer)
  ""
  (interactive "P")
  (let ((sql-sqlite-program sql-linx-program)
        (sql-sqlite-options sql-linx-options)
        (sql-sqlite-login-params '((database :file nil :must-match t))))
    (sql-product-interactive 'sql-sqlite buffer)))

(provide 'sql-linx)
;;; sql-linx.el ends here
