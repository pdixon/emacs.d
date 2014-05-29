;;; pd-window-extras.el --- Various helpers for working with windows.

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
(defun pd/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;;;###autoload
(defun pd/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

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
(defun pd/setup-windows ()
  (interactive)
  (require 'window-number)
  (delete-other-windows)
  (let ((b1 (current-buffer))
        (b2 (other-buffer)))
    (if (> (frame-width) (* 2 80))
        (split-window-horizontally))
    (if (> (frame-width) (* 3 80))
        (progn (split-window-horizontally)
               (split-window-vertically)))
    (balance-windows)
    (if (< 3 (count-windows))
        (progn
          (window-number-select 1)
          (deft)
          (window-number-select 2)
          (org-agenda 0 "w")
          (window-number-select 3)
          (switch-to-buffer b1)
          (window-number-select 4)
          (switch-to-buffer b2))
      (if (< 1 (count-windows))
          (progn
            (window-number-select 1)
            (switch-to-buffer b1)
            (window-number-select 2)
            (switch-to-buffer b2))))))

(defun pd/toggle-just-one-window ()
  "Switch from the current window setup to just one window or back."
  (interactive)
  (if (< 1 (count-windows))
      (progn
        (window-configuration-to-register ?u)
        (delete-other-windows))
    (jump-to-register ?u)))

(provide 'pd-window-extras)
;;; pd-window-extras.el ends here
