;;; hg-commit-mode.el --- A major mode for editing hg commit messages.

;; Copyright (C) 2011-13  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: convenience vc git

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

;; A major mode for editing hg commit messages.

;;; Code:

;;; Options
;;;; Variables

(defgroup hg-commit nil
  "Mode for editing hg commit messages"
  :prefix "hg-commit-"
  :group 'tools)

(defcustom hg-commit-summary-max-length 60
  "Fontify characters beyond this column in summary lines as errors."
  :group 'hg-commit
  :type 'number)

(defcustom hg-commit-fill-column 72
  "Automatically wrap commit message lines beyond this column."
  :group 'hg-commit
  :type 'number)

;;;; Faces

(defgroup hg-commit-faces nil
  "Faces for highlighting hg commit messages."
  :prefix "hg-commit-"
  :group 'hg-commit
  :group 'faces)

(defface hg-commit-text-face
  '((t (:inherit default)))
  "Face used to highlight body text in commit messages."
  :group 'hg-commit-faces)

(defface hg-commit-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face used to highlight comments in commit messages."
  :group 'hg-commit-faces)

(defface hg-commit-summary-face
  '((t (:inherit default)))
  "Face used to highlight the summary of a commit message."
  :group 'hg-commit-faces)

(defface hg-commit-summary-overlength-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight that over-length part of the summary of a commit message."
  :group 'hg-commit-faces)

(defface hg-commit-non-empty-second-line-face
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight the the second line of a commit message isn't empty."
  :group 'hg-commit-faces)

;;; Keymap

(defvar hg-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")              'hg-commit-commit)
    (define-key map (kbd "C-c C-k")              'hg-commit-cancel)
    (define-key map (kbd "C-c M-s")              'hg-commit-save-message)
    (define-key map (kbd "M-p")                  'hg-commit-prev-message)
    (define-key map (kbd "M-n")                  'hg-commit-next-message)
    (define-key map [remap server-edit]          'hg-commit-commit)
    (define-key map [remap kill-buffer]          'hg-commit-cancel)
    (define-key map [remap ido-kill-buffer]      'hg-commit-cancel)
    (define-key map [remap iswitchb-kill-buffer] 'hg-commit-cancel)
    map)
  "Key map used by `hg-commit-mode'.")

;;; Committing

(defun hg-commit-commit ()
  "Save the commit message and commit it."
  (interactive)
  (save-buffer)
  (if (hg-commit-buffer-clients)
      (server-edit)
    (kill-buffer)))

(defun hg-commit-cancel ()
  "Cancel the commit."
  (interactive)
  (save-buffer)
  (remove-hook 'kill-buffer-hook 'server-kill-buffer t)
  (let ((clients (hg-commit-buffer-clients)))
      (if clients
          (dolist (client clients)
            (server-send-string client "-error Commit aborted by user")
            (delete-process client))
        (kill-buffer)))
  (message "Commit aborted."))

(defun hg-commit-buffer-clients ()
  (and (fboundp 'server-edit)
       (boundp 'server-buffer-clients)
       server-buffer-clients))

;;; Fontlock

(defvar hg-commit-font-lock-keywords
  '(("\\`\\(.\\{,50\\}\\)\\(.*?\\)\n\\(.*\\)$"
     (1 'hg-commit-summary-face)
     (2 'hg-commit-summary-overlength-face)
     (3 'hg-commit-non-empty-second-line-face))
    ("^\\(\\(HG:\\)\\).*$"
     (0 'hg-commit-comment-face))
    (".*"
     (0 'hg-commit-text-face))))

;;; Mode

;;;###autoload
(define-derived-mode hg-commit-mode text-mode "Hg Commit"
  "Major mode for editing hg commit messages.

\\{hg-commit-mode-map}"
  (set (make-local-variable 'font-lock-multiline) t)
  (setq font-lock-defaults '(hg-commit-font-lock-keywords t))
  (set (make-local-variable 'comment-start) "HG:")
  (setq fill-column hg-commit-fill-column)
  (set (make-local-variable 'comment-start-skip) "^\\(\\(HG:\\)\\)\s")
  (set (make-local-variable 'comment-end) ""))


;;;###autoload
(when (boundp 'session-mode-disable-list)
  (add-to-list 'session-mode-disable-list 'hg-commit-mode))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("hg-editor-.*\\.txt" . hg-commit-mode))

(provide 'hg-commit-mode)
;;; hg-commit-mode.el ends here
