;;; 70window-handling.el --- How to carve up a frame.

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

;;; I'd rather reuse a window than have things split on me.
(setq split-height-threshold nil)
(setq split-width-threshold nil)

(require 'cl)
(defun first-matching-buffer (predicate)
  "Return PREDICATE applied to the first buffer where PREDICATE applied to the
   buffer yields a non-nil value."
  (loop for buf in (buffer-list)
        when (with-current-buffer buf (funcall predicate buf))
        return (with-current-buffer buf (funcall predicate buf))))

(defun show (buffer)
  (set-window-buffer (selected-window) buffer))

(require 'window-number)
(defun my-windows ()
  (interactive)
  (let ((current-project
         (first-matching-buffer (lambda (x) (ignore-errors (eproject-name))))))
  (deft)
  (org-agenda 0 "w")
  (delete-other-windows)
  (if (> (frame-width) (* 2 80))
         (split-window-horizontally))
  (if (> (frame-width) (* 3 80))
      (progn (split-window-horizontally)
             (split-window-vertically)))
  (balance-windows)
  (window-number-select 1)
  (set-window-buffer (selected-window) "*Deft*")
  (window-number-select 2)
  (set-window-buffer (selected-window) "*Org Agenda*")
  (let ((cur))
    (loop for i in '(3 4)
          do
          (window-number-select i)
          (show (first-matching-buffer
                 (lambda (x) (and (equal (ignore-errors (eproject-name))
                                         current-project)
                                  (not (equal cur (buffer-name x)))
                                  x))))
          (setf cur (buffer-name (window-buffer)))))))

(provide '70window-handling)
;;; 70window-handling.el ends here
