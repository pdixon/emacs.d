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

(defface commit-msg-text-face
  '((t (:inherit default)))
  "Face used to highlight body text in commit messages.")

(defface commit-msg-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to highlight comments in commit messages.")

(defface commit-msg-summary-face
  '((t (:inherit default)))
  "Face used to highlight the summary of a commit message.")

(defface commit-msg-summary-overlength-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight that overlength part of the summary of a commit message.")

(defface commit-msg-non-empty-second-line-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight the the second line of a commit message isn't empty.")

(defvar commit-msg-font-lock-keywords
  '(("\\`\\(.\\{,50\\}\\)\\(.*?\\)\n\\(.*\\)$"
     (1 'commit-msg-summary-face)
     (2 'commit-msg-summary-overlength-face)
     (3 'commit-msg-non-empty-second-line-face))
    ("^\\(\\(HG:\\)\\|\\(#\\)\\).*$"
     (0 'commit-msg-comment-face))
    (".*"
     (0 'commit-msg-text-face))))

(defun commit-msg-commit ()
  "Save the commit message and commit it."
  (interactive)
  (save-buffer)
  (kill-buffer))

(defun commit-msg-cancel ()
  "Throw away the current message (and thus throw away the commit)."
  (interactive)
  (kill-buffer))

(define-derived-mode commit-msg-mode text-mode "Commit-Msg"
  "Major mode for editting git and hg commit messages.

\\{commit-msg-mode-map}"
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(commit-msg-font-lock-keywords t))
  (set (make-local-variable 'comment-start-skip) "^\\(\\(HG:\\)\\|\\(#\\)\\)\s")
  (set (make-local-variable 'comment-start) "HG:\|#")
  (set (make-local-variable 'comment-end) "")
)

(define-key commit-msg-mode-map
  (kbd "C-c C-c") 'commit-msg-commit)
(define-key commit-msg-mode-map
  (kbd "C-c C-k") 'commit-msg-cancel)

;;;###autoload
(when (boundp 'session-mode-disable-list)
  (add-to-list 'session-mode-disable-list 'commit-msg-mode))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG" . commit-msg-mode))
(add-to-list 'auto-mode-alist
             '("hg-editor-.*\\.txt" . commit-msg-mode))

(provide 'commit-msg-mode)
;;; commit-msg-mode.el ends here
