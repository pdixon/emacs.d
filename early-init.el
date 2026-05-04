;;; early-init.el --- early init                     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Phillip Dixon

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(if (eq system-type 'darwin)
    (progn
      (setenv "PATH" (concat "/opt/homebrew/bin" ":" (getenv "PATH")))
      (add-to-list 'exec-path "/opt/homebrew/bin")))

(setq package-enable-at-startup nil)
(setq package-selected-packages nil)

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.3f seconds (time to emacs-startup-hook: %.3fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 100)

(provide 'early-init)
;;; early-init.el ends here
