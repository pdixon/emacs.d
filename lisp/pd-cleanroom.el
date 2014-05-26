;;; pd-cleanroom.el --- Focus on a single buffer     -*- lexical-binding: t; -*-

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

(defun pd-cleanroom-adjust-window (&optional arg)
  (let ((windows (window-list)))
    (dolist (window windows)
      (unless (window-minibuffer-p window)
        (if (not arg)
            (let* ((current-width (frame-total-cols))
                   (margin (/ (- current-width 100) 2)))
              (set-window-margins window margin margin))
          (set-window-margins window nil nil))))))

(defvar pd-cleanroom-mode nil)

(define-minor-mode pd-cleanroom-mode
  :init-value nil
  :global t
  :variable pd-cleanroom-mode
  :group 'editing-basics
  (toggle-frame-fullscreen)
  (if (not pd-cleanroom-mode)
      (progn
        (menu-bar-mode 1)
        (pd-cleanroom-adjust-window t)
        (remove-hook 'window-configuration-change-hook 'pd-cleanroom-adjust-window))
    (progn
      (menu-bar-mode -1)
      (delete-other-windows)
      (add-hook 'window-configuration-change-hook 'pd-cleanroom-adjust-window)
      (pd-cleanroom-adjust-window nil))))

(provide 'pd-cleanroom)
;;; pd-cleanroom.el ends here
