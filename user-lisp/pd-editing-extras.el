;;; pd-editing-extras.el --- My editing helper functions

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

(defvar cycle-spacing--context nil
  "Store context used in consecutive calls to `cycle-spacing' command.
The first time this function is run, it saves the original point
position and original spacing around the point in this
variable.")

(defun cycle-spacing (&optional n preserve-nl-back single-shot)
  "Manipulate spaces around the point in a smart way.

When run as an interactive command, the first time it's called
in a sequence, deletes all spaces and tabs around point leaving
one (or N spaces).  If this does not change content of the
buffer, skips to the second step:

When run for the second time in a sequence, deletes all the
spaces it has previously inserted.

When run for the third time, returns the whitespace and point in
a state encountered when it had been run for the first time.

For example, if buffer contains \"foo ^ bar\" with \"^\" donating the
point, calling `cycle-spacing' command will replace two spaces with
a single space, calling it again immediately after, will remove all
spaces, and calling it for the third time will bring two spaces back
together.

If N is negative, delete newlines as well.  However, if
PRESERVE-NL-BACK is t new line characters prior to the point
won't be removed.

If SINGLE-SHOT is non-nil, will only perform the first step.  In
other words, it will work just like `just-on-space' command."
  (interactive "*p")
  (let ((orig-pos        (point))
        (skip-characters (if (and n (< n 0)) " \t\n\r" " \t"))
        (n               (abs (or n 1))))
    (skip-chars-backward (if preserve-nl-back " \t" skip-characters))
    (constrain-to-field nil orig-pos)
    (cond
     ;; Command run for the first time or single-shot is non-nil
     ((or single-shot
          (not (equal last-command this-command))
          (not cycle-spacing--context))
      (let* ((start (point))
             (n     (- n (skip-chars-forward " " (+ n (point)))))
             (mid   (point))
             (end   (progn
                      (skip-chars-forward skip-characters)
                      (constrain-to-field nil orig-pos t))))
        (setq cycle-spacing--context  ;; Save for later
              ;; Special handling for case where there was no space at all
              (unless (= start end)
                (cons orig-pos (buffer-substring start (point)))))
        ;; If this run causes no change in buffer content, delete all spaces,
        ;; otherwise delete all excees spaces.
        (delete-region (if (and (not single-shot) (zerop n) (= mid end))
                           start mid) end)
        (dotimes (_ n)
          (insert ?\s))))

     ;; Command run for the second time
     ((not (equal orig-pos (point)))
      (delete-region (point) orig-pos))

     ;; Command run for the third time
     (t
      (insert (cdr cycle-spacing--context))
      (goto-char (car cycle-spacing--context))
      (setq cycle-spacing--context nil)))))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
(interactive)
(let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
  (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning)
            pos2 (region-end))
    (setq pos1 (car (bounds-of-thing-at-point 'word))
          pos2 (cdr (bounds-of-thing-at-point 'word))))

  (when (not (eq last-command this-command))
    (save-excursion
      (goto-char pos1)
      (cond
       ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
       ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
       ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
       (t (put this-command 'state "all lower")))))

  (cond
   ((string= "all lower" (get this-command 'state))
    (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
   ((string= "init caps" (get this-command 'state))
    (upcase-region pos1 pos2) (put this-command 'state "all caps"))
   ((string= "all caps" (get this-command 'state))
    (downcase-region pos1 pos2) (put this-command 'state "all lower")))))

(defun my-increment ()
  "Transform the number under the point to Wizard DB string ref.
   Note current only works for PM databases."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (+ 1 (string-to-number (match-string 0))))))

(defun transpose-dwim (arg)
 "Execute the appropriate transpose based on where the point is.

If the point is in a word do a transpose character. If it is between
words do a transpose word. If it is on the start of a line, do a
transpose line.
"
 (interactive "*p")
 (cond ((bolp) (transpose-lines arg))
       ((looking-at "[[:space:]]") (transpose-words arg))
       (t (transpose-chars arg))))

(provide 'pd-editing-extras)
;;; pd-editing-extras.el ends here
