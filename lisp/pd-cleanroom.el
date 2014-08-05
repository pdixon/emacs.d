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

;;;###autoload
(define-minor-mode pd-cleanroom-mode
  :init-value nil
  :global t
  :group 'editing-basics
  (toggle-frame-fullscreen)
  (if (not pd-cleanroom-mode)
      (progn
        (menu-bar-mode 1)
        (pd-centered-window-mode -1)
        )
    (progn
      (menu-bar-mode -1)
      (delete-other-windows)
      (pd-centered-window-mode 1))))

(provide 'pd-cleanroom)
;;; pd-cleanroom.el ends here
