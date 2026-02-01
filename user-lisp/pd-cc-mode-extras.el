;;; pd-cc-mode-extras.el --- My helpers for cc-mode

;; Copyright (C) 2012  Phillip Dixon

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

(defun pd/toggle-header (&optional in-other-window)
  ""
  (interactive "P")
  (ff-get-other-file in-other-window))

(defun pd/toggle-test (&optional in-other-window)
  ""
  ;; This is based on ff-find-the-other-file, but without all the
  ;; customizable variables
  (interactive "P")
  (let (match     ;; matching regexp for this file.
        pos       ;; where the filenames start matching.
        suffixes  ;; replacement regexps for the matching regexp.
        format    ;; what we have to match.
        found     ;; name of the file or buffer found - nil if none.
        stub      ;; name of file without extension.
        (dirs '("../*")) ;; where to look for the file.
        (alist '(("Tests\\.m\\'" (".m"))
                 ("\\.m\\'" ("Tests.m")))) ;; candidate extensions.
        (fname (file-name-nondirectory buffer-file-name)) ;; basename
        )
    (setq match (car alist))
    ;; find the table entry corresponding to this file
    (setq pos (ff-string-match (car match) fname))
    (while (and match (if (and pos (>= pos 0)) nil (not pos)))
      (setq alist (cdr alist))
      (setq match (car alist))
      (setq pos (ff-string-match (car match) fname)))
    ;; otherwise, suffixes contains what we need
    (setq suffixes (car (cdr match))
          found nil)
    ;; otherwise build our filename stub
    (cond
     ;; get around the problem that 0 and nil both mean false!
     ((= pos 0)
      (setq format "")
      (setq stub "")
      )

     (t
      (setq format (concat "\\(.+\\)" (car match)))
      (string-match format fname)
      (setq stub (substring fname (match-beginning 1) (match-end 1)))
      ))

    (setq found
          (ff-get-file dirs
                       stub
                       suffixes
                       in-other-window))))

(provide 'pd-cc-mode-extras)
;;; pd-cc-mode-extras.el ends here
