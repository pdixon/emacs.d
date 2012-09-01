;;; pd-eproject-extensions.el --- My eproject extensions

;; Copyright (C) 2012  Phillip Dixon

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

(require 'eproject)

(defun eproject-compile-dwim ()
  ""
  (interactive)
  (let ((comp-buffer-name (concat "*compilation: " (eproject-name) "*"))
        (default-directory (eproject-root)))
    (if (get-buffer comp-buffer-name)
        (with-current-buffer comp-buffer-name
          (recompile))
      (progn
        (compile compile-command)
        (with-current-buffer "*compilation*"
          (rename-buffer comp-buffer-name))))))

(defun ibuffer-eproject-generate-filter-groups-by-name ()
  "Create a set of ibuffer filter groups based on the eproject root dirs of buffers"
  (mapcar (lambda (project-name)
            (cons (format "%s" project-name)
                  `((eproject . ,project-name))))
          (eproject-project-names)))

(defun ibuffer-eproject-set-filter-groups-by-name ()
  "Set the current filter groups to filter by eproject root dir."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-eproject-generate-filter-groups-by-name))
  (ibuffer-update nil t))

(defun all-projects-ibuffer (prefix)
  "Open an IBuffer window showing all buffers by project."
  (interactive "p")
  (ibuffer nil "*Projects*" nil nil nil (ibuffer-eproject-generate-filter-groups-by-name)))


(provide 'pd-eproject-extensions)
;;; pd-eproject-extensions.el ends here
