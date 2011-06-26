;;; (pd-markdown-mode.el) --- 

;; Copyright (C) (2011)  Phillip Dixon

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

(define-derived-mode pd-markdown-mode outline-mode "Markdown"
  "Phil's Markdown mode.

The following commands are available:

\\{pd-markdown-mode-map}"
  ;; Setup outline stuff
  ;; Paragraphs and auto-filling
  (setq indent-line-function 'pd-markdown-indent-line-function)
)

;;; Paragraph filling
(defun pd-markdown-indent-line-function ()
  "Indent line based on context."
  (interactive)
  (beginning-of-line 1)
  (cond
   ;; Headings
   ((looking-at "#+ ") (setq column 0))
   ;; This is a boring line. We need to look further back.
   (t
    (beginning-of-line 0)
    (while (not (bobp))
      (beginning-of-line 0))
    (cond
     ;; There is a heading.
     ((looking-at "#+[ \t]+") (setq column 0))
    ))
  ;;; We know where to go. Make it so.
  (goto-char pos)
  (if (<= (current-column) (current-indentation))
      (indent-line-to column)
    (save-excursion (indent-line-to column))))
  (move-to-column column))

(defun pd-markdown-adaptive-fill-function ()
  "Return a fill prefix for markdown files."
)

(provide 'pd-markdown-mode)
;;; (pd-markdown-mode.el) ends here
