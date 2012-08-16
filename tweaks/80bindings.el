;; Phil's Bindings
(global-set-key "\C-c+" 'my-increment)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "M-SPC") 'shrink-whitespaces)
(global-set-key (kbd "C-t") 'transpose-dwim)
(global-set-key (kbd "M-c") 'toggle-letter-case)
(global-set-key (kbd "C-z") 'iy-go-to-char)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-h i") 'info-apropos)

;;;; Helper functions from Xahlee's ergo bindings (http://xahlee.org).
;;; TEXT SELECTION RELATED

(defun shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line does not contain non-white space chars, then remove blank lines to just one.
If current line contains non-white space chars, then shrink any whitespace char surrounding cursor to just one space.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let (
        cursor-point
        line-has-meat-p  ; current line contains non-white space chars
        spaceTabNeighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        line-begin-pos line-end-pos
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq cursor-point (point))

      (setq spaceTabNeighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil) )
      (move-beginning-of-line 1) (setq line-begin-pos (point) )
      (move-end-of-line 1) (setq line-end-pos (point) )
      ;;       (re-search-backward "\n$") (setq line-begin-pos (point) )
      ;;       (re-search-forward "\n$") (setq line-end-pos (point) )
      (setq line-has-meat-p (if (< 0 (count-matches "[[:graph:]]" line-begin-pos line-end-pos)) t nil) )
      (goto-char cursor-point)

      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char cursor-point)      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point))
      )

    (if line-has-meat-p
        (let (deleted-text)
          (when spaceTabNeighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " ") ) ) )

      (progn
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n")
        (delete-blank-lines)
        )
      ;; todo: possibly code my own delete-blank-lines here for better efficiency, because delete-blank-lines seems complex.
      )
    )
  )

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

;;;; And some I've put together myself.

;;;; And some other bits and pieces.
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))
