;;; wizard-db.el --- A major mode for editting DCL Wizard Database Files.

;; Copyright (C) 2009 by Phillip Dixon

;; Author: Phillip Dixon
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(defun wizard-db-to-str-ref ()
  "Transform the number under the point to Wizard DB string ref.
   Note current only works for PM databases."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (+ 8388608 (string-to-number (match-string 0))))))

(defvar wizard-db-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ct" 'wizard-db-to-str-ref)
    map)
    "Keymap for `wizard-db-mode'.")

(define-derived-mode wizard-db-mode
  xml-mode "Wizard DB"
  "A major mode for editing wizard .xmd files."
  (auto-fill-mode 0))

(provide 'wizard-db-mode)
;;; wizard-db.el ends here.
