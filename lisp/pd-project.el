;;; pd-project.el --- Project Operations -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25"))

;; Copyright (C) 2013-2016  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: tools, vc, matching, convenience

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

;;; Code:

(require 'compile)
(require 'project)
(require 'subr-x)

(defgroup pd-project nil
  "PD Project settings."
  :group 'programming)

(defcustom pd-project-todo-regexp
  '("TODO" "HACK" "FIXME")
  "A list of terms for `pd-project-todo' to search for."
  :group 'pd-proj
  :type '(repeat string))

;;;###autoload
(defun pd-project-compile ()
  "Call compile at the project root."
  (interactive)
  (when-let (root (nth 0 (project-roots (project-current))))
    (let ((comp-buffer-name (concat "*compilation: " root "*"))
          (default-directory root))
      (if (get-buffer comp-buffer-name)
          (with-current-buffer comp-buffer-name
            (recompile))
        (progn
          (compile compile-command)
          (with-current-buffer "*compilation*"
            (rename-buffer comp-buffer-name)))))))

;;;###autoload
(defun pd-project-todo ()
  "Find TODO type comments in the project.

`pd-project-todo-regexp' is used to determine what this function looks for."
  (interactive)
  (project-find-regexp (regexp-opt pd-project-todo-regexp)))

(provide 'pd-project)
;;; pd-project.el ends here
