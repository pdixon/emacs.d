;;; pd-cleanroom.el --- Focus on a single buffer     -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: convenience
;; Version: 0.0.1

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

(require 'pd-centered-window)

(defun pd-cleanroom--enter ()
  ""
  (pd-centered-window-mode 1))

(defun pd-cleanroom--leave ()
  ""
  (pd-centered-window-mode -1))

(defun pd-cleanroom--update ()
  ""
  (if (= (count-windows) 1)
      (pd-cleanroom--enter)
    (pd-cleanroom--leave)))

;;;###autoload
(define-minor-mode pd-cleanroom-mode
  ""
  :init-value nil
  :global t
  :group 'editing-basics
  (if (not pd-cleanroom-mode)
      (progn
        (remove-hook 'window-configuration-change-hook
                     'pd-cleanroom--update 'local)
        (pd-cleanroom--leave))
    (progn
      (add-hook 'window-configuration-change-hook
                'pd-cleanroom--update 'append 'local)
      (pd-cleanroom--update))))

(provide 'pd-cleanroom)
;;; pd-cleanroom.el ends here
