;;; commit-msg-mode.el --- A major mode for editting commit messages.

;; Copyright (C) 2011  Phillip Dixon

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

(defun commit-msg-commit ()
  "Save the commit message and commit it."
  (interactive)
  (save-buffer)
  (if (server-buffer-clients)
      (server-edit)
    (kill-buffer)))

(defun commit-msg-cancel ()
  "Throw away the current message (and thus commit)."
  (interactive)
  (if (server-buffer-clients)
      (server-edit)
    (kill-buffer)))

(define-derived-mode commit-msg-mode text-mode "Commit-Msg"
  "Major mode for editting git and hg commit messages.
\\{commit-msg-mode-map}"
)

(define-key commit-msg-mode-map
  (kbd "C-c C-c") 'commit-msg-commit)
(define-key commit-msg-mode-map
  (kbd "C-c C-k") 'commit-msg-cancel)

(provide 'commit-msg-mode)
;;; commit-msg-mode.el ends here
