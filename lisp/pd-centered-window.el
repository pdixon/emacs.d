;;; pd-centered-window.el --- Center windows when there's only one column  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: frames
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
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

(defun pd-centered-setup ()
  (add-hook 'window-configuration-change-hook
            'pd-centered-window-config-changed)
  (pd-centered-window-config-changed))

(defun pd-centered-teardown ()
  (remove-hook 'window-configuration-change-hook
               'pd-centered-window-config-changed)
  (pd-centered-window-config-changed))

; from http://stackoverflow.com/a/25083568/137121
(defun any-horizontal-splits-p ()
  "Return t if any windows have been split horizontally."
  (when (member nil (mapcar #'window-full-width-p (window-list))) t))

(defun pd-centered-window-config-changed ()
  (if (or (any-horizontal-splits-p)
          (null pd-centered-window-mode))
      (pd-centered-adjust-window nil)
    (pd-centered-adjust-window t)))

(defun pd-centered-adjust-window (arg)
  (let ((windows (window-list)))
    (dolist (window windows)
      (unless (window-minibuffer-p window)
        (if (not (null arg))
            (let* ((current-width (frame-total-cols))
                   (margin (/ (- current-width 100) 2)))
              (set-window-margins window margin margin))
          (set-window-margins window nil nil))))))

;;;###autoload
(define-minor-mode pd-centered-window-mode
  ""
  :init-value nil
  :global t
  (if pd-centered-window-mode
      (pd-centered-setup)
    (pd-centered-teardown)))

(provide 'pd-centered-window)
;;; pd-centered-window.el ends here
