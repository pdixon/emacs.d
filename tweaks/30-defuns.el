(defun my-toggle-fullscreen () 
  (interactive) 
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) 
                                           nil 
                                           'fullboth)))
(defun my-kill-word ()
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-syntax-backward "w_")
      (setq p1 (point))
      (skip-syntax-forward "w_")
      (setq p2 (point))
      (kill-region p1 p2))))

(defun my-copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun my-isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

(defun my-search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
	 (match-beginning 0)
       cur))))

(defun my-search-word-forward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
	 (match-beginning 0)
       cur))))

(defun my-to-wizard-str-ref ()
  "Transform the number under the point to Wizard DB string ref.
   Note current only works for PM databases."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (+ 8388608 (string-to-number (match-string 0))))))


(defun my-increment ()
  "Transform the number under the point to Wizard DB string ref.
   Note current only works for PM databases."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (+ 1 (string-to-number (match-string 0))))))

