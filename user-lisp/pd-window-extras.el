;;; pd-window-extras.el --- Various helpers for working with windows.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Phillip Dixon

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

;; The following are from http://whattheemacsd.com

;;;###autoload
(defun pd/vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

;;;###autoload
(defun pd/hsplit-last-buffer ()
  (interactive)
   (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

;;;###autoload
(defun pd/toggle-window-dedicated ()
  "Toggle whether this window is dedicated to this buffer."
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;;;###autoload
(defun pd/toggle-just-one-window ()
  "Switch from the current window setup to just one window or back."
  (interactive)
  (if (< 1 (count-windows))
      (progn
        (window-configuration-to-register ?u)
        (delete-other-windows))
    (jump-to-register ?u)))

(defun pd--split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    ""
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(defalias 'pd-split-other-buffer-vertically (pd--split-window-func-with-other-buffer 'split-window-vertically))
(defalias 'pd-split-other-buffer-horizontally (pd--split-window-func-with-other-buffer 'split-window-horizontally))

(provide 'pd-window-extras)
;;; pd-window-extras.el ends here
