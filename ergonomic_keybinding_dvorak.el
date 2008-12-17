;-*- coding: utf-8 -*-
;; ergonomic_keybinding_dvorak.el -- A ergonomic keybinding for Dvorak keyboard.

;-*- coding: utf-8 -*-
;; Copyright © 2007, 2008 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Keywords: qwerty, dvorak, keybinding, ergonomic

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; DESCRIPTION

;; This keybinding set puts the most frequently used emacs keyboard
;; shortcuts into the most easy-to-type spots.
;;
;; For complete detail, see: 
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

;;; INSTALL

;; Place this file in your favorite directory, such as “~/emacs/”.
;; Then, place the following code in your emacs init file (the "~/.emacs"):
;; (load-file "~/emacs/ergonomic_keybinding_qwerty.el")
;; ; or 
;; (load-file "~/emacs/ergonomic_keybinding_dvorak.el")
;; Then, restart emacs.

;;; History:

;-*- coding: utf-8 -*-
;; version 4.0.1, 2008-09-23. Fixed C-o in dired mode.
;; version 4, 2008-09-21. Unbind almost all Meta-‹key› and Ctrl-‹key› space bindings. Added about 11 commands, such as next-user-buffer, close-current-buffer etc.
;; version 3.6, 2008-09-18. Reclaimed keybindings in text-mode.
;; version 3.5, 2008-09-16. Reclaimed keybindings in ibuffer.
;; version 3.4, 2008-09-06. Fixed key bindings in the Meta-‹key› space in about 10 modes.
;; version 3.3, 2008-09-05. Fixed cua-mode-hook by setting symbol property 'CUA to nil, so that a bunch of no-select-* functions kludge is no longer needed.
;; version 3.2, 2008-09-02. Moved cua fix functions to modern_operations.el. The functinos are: no-select-cua-scroll-down, no-select-cua-scroll-up, no-select-backward-paragraph, no-select-forward-paragraph, no-select-beginning-of-buffer, no-select-end-of-buffer, no-select-move-end-of-line.
;; version 3.1, 2008-09-02. Added just-one-space, delete-blank-lines. Added fill-paragraph, unfill-paragraph. Added comment-dwim.
;; version 3, 2008-08-31. Added isearch. Added redo, kill-line-backward, shell-command. Added bug fix for cua-mode. Now, commands with Shift keys won't go into a selection mode.
;; version 2, 2008-08-29. Somewhat major change. Positions for undo, cut, copy, paste, paste previous, has been moved. Added delete-char, delete-backward-char, kill-word, backward-kill-word. Removed the now redundant binding of kill-word and backward-kill-word using the backspace key. Removed the other-frame binding.
;; version 1.1, 2007-12-18. changed keycode to consistantly use kbd syntax. Fixed a scroll-up and scroll-down mixup.
;; version 1.0, 2007-08-01. first version.


;;; Code:

;;; --------------------------------------------------
;;; UNBIND DEFAULT KEYS

;-*- coding: utf-8 -*-
(global-unset-key (kbd "M-1")) ; digit-argument
(global-unset-key (kbd "M-2")) ; digit-argument
(global-unset-key (kbd "M-3")) ; digit-argument
(global-unset-key (kbd "M-4")) ; digit-argument
(global-unset-key (kbd "M-5")) ; digit-argument
(global-unset-key (kbd "M-6")) ; digit-argument
(global-unset-key (kbd "M-7")) ; digit-argument
(global-unset-key (kbd "M-8")) ; digit-argument
(global-unset-key (kbd "M-9")) ; digit-argument
(global-unset-key (kbd "M-0")) ; digit-argument

(global-unset-key (kbd "M-a")) ; backward-sentence
(global-unset-key (kbd "M-b")) ; backward-word
(global-unset-key (kbd "M-c")) ; capitalize-word
(global-unset-key (kbd "M-d")) ; kill-word
(global-unset-key (kbd "M-e")) ; forward-sentence
(global-unset-key (kbd "M-f")) ; forward-word
(global-unset-key (kbd "M-g")) ; (prefix)
(global-unset-key (kbd "M-h")) ; mark-paragraph
(global-unset-key (kbd "M-i")) ; tab-to-tab-stop
(global-unset-key (kbd "M-j")) ; indent-new-comment-line
(global-unset-key (kbd "M-k")) ; kill-sentence
(global-unset-key (kbd "M-l")) ; downcase-word
(global-unset-key (kbd "M-m")) ; back-to-indentation
(global-unset-key (kbd "M-n")) ; nil
(global-unset-key (kbd "M-o")) ; nil
(global-unset-key (kbd "M-p")) ; nil
(global-unset-key (kbd "M-q")) ; fill-paragraph
(global-unset-key (kbd "M-r")) ; move-to-window-line
(global-unset-key (kbd "M-s")) ; nil
(global-unset-key (kbd "M-t")) ; transpose-words
(global-unset-key (kbd "M-u")) ; upcase-word
(global-unset-key (kbd "M-v")) ; scroll-down
(global-unset-key (kbd "M-w")) ; kill-ring-save
(global-unset-key (kbd "M-x")) ; execute-extended-command
(global-unset-key (kbd "M-y")) ; yank-pop
(global-unset-key (kbd "M-z")) ; zap-to-char

(global-unset-key (kbd "M-\\")) ; delete-horizontal-space
(global-unset-key (kbd "M-@")) ; mark-word
(global-unset-key (kbd "M--")) ; negative-argument
(global-unset-key (kbd "M-<")) ; beginning-of-buffer
(global-unset-key (kbd "M->")) ; end-of-buffer

(global-unset-key (kbd "C-1")) ; digit-argument
(global-unset-key (kbd "C-2")) ; digit-argument
(global-unset-key (kbd "C-3")) ; digit-argument
(global-unset-key (kbd "C-4")) ; digit-argument
(global-unset-key (kbd "C-5")) ; digit-argument
(global-unset-key (kbd "C-6")) ; digit-argument
(global-unset-key (kbd "C-7")) ; digit-argument
(global-unset-key (kbd "C-8")) ; digit-argument
(global-unset-key (kbd "C-9")) ; digit-argument
(global-unset-key (kbd "C-0")) ; digit-argument

(global-unset-key (kbd "C-a")) ; move-beginning-of-line
(global-unset-key (kbd "C-b")) ; backward-char
;(global-unset-key (kbd "C-c")) ; (prefix)
(global-unset-key (kbd "C-d")) ; delete-char
(global-unset-key (kbd "C-e")) ; move-end-of-line
(global-unset-key (kbd "C-f")) ; forward-char
;(global-unset-key (kbd "C-g")) ; keyboard-quit
;(global-unset-key (kbd "C-h")) ; (prefix)
;(global-unset-key (kbd "C-i")) ; indent-for-tab-command; this is tab key
(global-unset-key (kbd "C-j")) ; newline-and-indent
(global-unset-key (kbd "C-k")) ; kill-line
(global-unset-key (kbd "C-l")) ; recenter
;(global-unset-key (kbd "C-m")) ; newline-and-indent; This is the Return key
(global-unset-key (kbd "C-n")) ; next-line
(global-unset-key (kbd "C-o")) ; open-line
(global-unset-key (kbd "C-p")) ; previous-line
;(global-unset-key (kbd "C-q")) ; quote-insert
(global-unset-key (kbd "C-r")) ; isearch-backward
(global-unset-key (kbd "C-s")) ; isearch-forward
(global-unset-key (kbd "C-t")) ; transpose-chars
;(global-unset-key (kbd "C-u")) ; universal-argument
(global-unset-key (kbd "C-v")) ; scroll-up
(global-unset-key (kbd "C-w")) ; kill-region
;(global-unset-key (kbd "C-x")) ; (prefix)
(global-unset-key (kbd "C-y")) ; yank
(global-unset-key (kbd "C-z")) ; iconify-or-deiconify-frame

(global-unset-key (kbd "C-/")) ; undo
(global-unset-key (kbd "C-_")) ; undo
(global-unset-key (kbd "C-<backspace>")) ; backward-kill-word

(global-unset-key (kbd "C-<prior>")) ; scroll-right
(global-unset-key (kbd "C-<next>")) ; scroll-left

;(global-unset-key (kbd "C-x C-f")) ; find-file
;(global-unset-key (kbd "C-x d")) ; dired
;(global-unset-key (kbd "C-x C-d")) ; list-directory


;;; --------------------------------------------------
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-n") 'forward-char)
(global-set-key (kbd "M-c") 'previous-line)
(global-set-key (kbd "M-t") 'next-line)

;; Move by word
(global-set-key (kbd "M-H") 'backward-word) ; was (prefix)
(global-set-key (kbd "M-N") 'forward-word)

;; Move by paragraph
(global-set-key (kbd "M-g") 'backward-sentence)
(global-set-key (kbd "M-r") 'forward-sentence)

;; Move by paragraph
(global-set-key (kbd "M-G") 'backward-paragraph)
(global-set-key (kbd "M-R") 'forward-paragraph)

;; Move to beginning/ending of line
(global-set-key (kbd "M-d") 'move-beginning-of-line)
(global-set-key (kbd "M-D") 'move-end-of-line)

;; Move by screen (page up/down)
(global-set-key (kbd "M-T") 'scroll-up)
(global-set-key (kbd "M-C") 'scroll-down)

;; Move to beginning/ending of file
(global-set-key (kbd "M-f") 'beginning-of-buffer)
(global-set-key (kbd "M-F") 'end-of-buffer)

;; isearch
(global-set-key (kbd "M-s") 'isearch-forward)
(global-set-key (kbd "M-S") 'isearch-backward)

(global-set-key (kbd "M-l") 'recenter)


;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(global-set-key (kbd "M-u") 'delete-char)
(global-set-key (kbd "M-e") 'delete-backward-char)

; Delete previous/next word.
(global-set-key (kbd "M-.") 'backward-kill-word)
(global-set-key (kbd "M-p") 'kill-word)

; Copy Cut Paste, Paste previous
(global-set-key (kbd "M-q") 'kill-region)
(global-set-key (kbd "M-j") 'kill-ring-save)
(global-set-key (kbd "M-k") 'yank)
(global-set-key (kbd "M-K") 'yank-pop)

;; undo and redo
(require 'redo "redo.elc" t)
(global-set-key (kbd "M-:") 'redo)
(global-set-key (kbd "M-;") 'undo)

; Kill line
(global-set-key (kbd "M-i") 'kill-line)
(global-set-key (kbd "M-I") 'kill-line-backward)

;;; Textual Transformation

(global-set-key (kbd "M-S-SPC") 'mark-paragraph)
(global-set-key (kbd "M-,") 'just-one-space)
(global-set-key (kbd "M-<") 'delete-blank-lines)
(global-set-key (kbd "M--") 'comment-dwim)

; Hard-wrap/un-hard-wrap paragraph
(global-set-key (kbd "M-'") 'fill-paragraph)
(global-set-key (kbd "M-\"") 'unfill-paragraph)

;;; EMACS'S SPECIAL COMMANDS

; Mark point.
(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-A") 'shell-command)

;;; WINDOW SPLITING
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'other-window) ; was prefix

;;; --------------------------------------------------
;;; STANDARD SHORTCUTS

;-*- coding: utf-8 -*-
;(global-set-key (kbd "C-n") 'new-empty-buffer) ; Open New File
;(global-set-key (kbd "C-o") 'find-file) ; Open
;(global-set-key (kbd "C-w") 'close-current-buffer) ; Close
;(global-set-key (kbd "C-s") 'save-buffer) ; Save
;(global-set-key (kbd "C-S-n") 'write-file) ; Save As.
;(global-set-key (kbd "C-p") 'print-buffer) ; Print
;(global-set-key (kbd "C-a") 'mark-whole-buffer) ; Select All

(global-set-key (kbd "<delete>") 'delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.

;;; --------------------------------------------------
;;; RECLAIM SOME BINDINGS

;; isearch
(add-hook 'isearch-mode-hook
 (lambda ()
 (define-key isearch-mode-map (kbd "M-s") 'isearch-repeat-forward)
 (define-key isearch-mode-map (kbd "M-S") 'isearch-repeat-backward)

 (define-key isearch-mode-map (kbd "M-n") 'forward-char) ; was isearch-ring-advance
 (define-key isearch-mode-map (kbd "M-p") 'kill-word) ; was isearch-ring-retreat

 (define-key isearch-mode-map (kbd "M-c") 'previous-line) ; was isearch-toggle-case-fold
 (define-key isearch-mode-map (kbd "M-r") 'forward-word) ; was isearch-toggle-regexp
 (define-key isearch-mode-map (kbd "M-e") 'delete-backward-char) ; was isearch-edit-string

 (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
 (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
 )
)

;; reclaim some bindings used in minibuffer
(define-key minibuffer-local-map (kbd "M-p") 'kill-word) ; was previous-history-element. Use ↑ key or f11.
(define-key minibuffer-local-map (kbd "M-n") 'forward-char) ; was next-history-element. Use ↓ key or f12.
(define-key minibuffer-local-map (kbd "M-r") 'forward-word) ; was previous-matching-history-element
(define-key minibuffer-local-map (kbd "M-s") 'isearch-forward) ; was next-matching-history-element
(define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
(define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
(define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
(define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element)


;; reclaim some binding used by shell mode and shell-command.
(add-hook 'comint-mode-hook
 (lambda ()
   (define-key comint-mode-map (kbd "M-p") 'kill-word) ; was comint-previous-input. Use Ctrl+↑ or f11
   (define-key comint-mode-map (kbd "M-n") 'forward-char) ; was comint-next-input. Use Ctrl+↓ or f12
   (define-key comint-mode-map (kbd "M-r") 'forward-word) ; was comint-previous-matching-input.
   (define-key comint-mode-map (kbd "M-s") 'isearch-forward) ; was comint-next-matching-input.

   (define-key comint-mode-map (kbd "<f11>") 'comint-previous-input)
   (define-key comint-mode-map (kbd "<f12>") 'comint-next-input)
   (define-key comint-mode-map (kbd "S-<f11>") 'comint-previous-matching-input)
   (define-key comint-mode-map (kbd "S-<f12>") 'comint-next-matching-input)
))

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "C-n") 'new-empty-buffer) ; was dired-next-line
  (define-key dired-mode-map (kbd "M-o") 'other-window) ; was dired-omit-mode
 ))

(add-hook 'Info-mode-hook
 (lambda ()
 (define-key Info-mode-map (kbd "M-n") 'forward-char) ; was clone-buffer
 (define-key Info-mode-map (kbd "M-s") 'isearch-forward) ; was Info-search; just press “s” instead for isearch-forward
 )
)

(add-hook 'text-mode-hook
 (lambda ()
 (define-key text-mode-map (kbd "M-s") 'isearch-forward) ; was center-line
 (define-key text-mode-map (kbd "M-S") 'isearch-backward) ; was center-paragraph
 )
)

;; prevent cua-mode from going into selection mode when commands with Shift key is used.
(add-hook 'cua-mode-hook
 (lambda ()
    (put 'cua-scroll-down 'CUA nil)
    (put 'cua-scroll-up 'CUA nil)
    (put 'backward-paragraph 'CUA nil)
    (put 'forward-paragraph 'CUA nil)
    (put 'beginning-of-buffer 'CUA nil)
    (put 'end-of-buffer 'CUA nil)
    (put 'move-end-of-line 'CUA nil)
   )
 )

;; reclaim some binding used by ibuffer.el
(add-hook 'ibuffer-mode-hook
 (lambda ()
   (define-key ibuffer-mode-map (kbd "M-g") 'backward-word) ; was ibuffer-jump-to-buffer. Use “j” instead.
   (define-key ibuffer-mode-map (kbd "M-p") 'kill-word) ; was ibuffer-backward-filter-group Use “←” instead.
   (define-key ibuffer-mode-map (kbd "M-n") 'forward-char) ; was ibuffer-forward-filter-group. Use “→” instead.
   (define-key ibuffer-mode-map (kbd "M-j") 'kill-ring-save) ; was ibuffer-jump-to-filter-group.
   (define-key ibuffer-mode-map (kbd "M-o") 'other-window) ; was ibuffer-visit-buffer-1-window
))

(add-hook 'html-mode-hook
 (lambda ()
 (define-key html-mode-map (kbd "M-s") 'isearch-forward) ; was center-line
 (define-key html-mode-map (kbd "M-S") 'isearch-backward) ; was center-paragraph
 )
)

(add-hook 'nxml-mode-hook
 (lambda ()
 (define-key nxml-mode-map (kbd "M-h") 'backward-char) ; was nxml-mark-paragraph
 (define-key nxml-mode-map (kbd "C-M-SPC") 'nxml-mark-paragraph)
 )
)

(add-hook 'w3m-mode-hook
 (lambda ()
  (define-key w3m-mode-map (kbd "M-a") 'execute-extended-command) ; was w3m-bookmark-add-this-url
  (define-key w3m-mode-map (kbd "M-g") 'backward-word) ; was goto-line
  (define-key w3m-mode-map (kbd "M-n") 'forward-char) ; was w3m-copy-buffer
  (define-key w3m-mode-map (kbd "M-l") 'recenter) ; was w3m-horizontal-recenter

  (define-key w3m-mode-map (kbd "M-i") 'kill-line) ; was w3m-save-image
  (define-key w3m-mode-map (kbd "M-k") 'yank) ; was w3m-cookie
))

(add-hook 'rcirc-mode-hook
 (lambda ()
  (define-key rcirc-mode-map (kbd "M-p") 'kill-word) ; was rcirc-insert-prev-input
  (define-key rcirc-mode-map (kbd "M-n") 'forward-char) ; was rcirc-insert-next-input
  (define-key rcirc-mode-map (kbd "<f11>") 'rcirc-insert-next-input)
  (define-key rcirc-mode-map (kbd "<f12>") 'rcirc-insert-prev-input)
 ))

(add-hook 'awk-mode-hook
 (lambda ()
  (define-key awk-mode-map (kbd "M-a") 'execute-extended-command) ; was c-beginning-of-statement
  (define-key awk-mode-map (kbd "M-e") 'delete-backward-char) ; was c-end-of-statement
 ))

;; nothing to fix: c-mode, c++-mode, java, sh, js, perl, php, python

;;; --------------------------------------------------
;;; FUNCTIONS

;-*- coding: utf-8 -*-

(setq mac-pass-command-to-system nil)
(delete-selection-mode t) ; turn on text selection highlighting and make typing override selected text

;;; TEXT TRANSFORMATION RELATED

(defun kill-line-backward ()
  "Kill text between the beginning of the line to the cursor position."
  (interactive)
  (kill-line 0)
;; (let (x1 x2)
;;     (setq x1 (point))
;;     (move-beginning-of-line 1)
;;     (setq x2 (point))
;;     (kill-region x1 x2))
  )

(defun unfill-paragraph ()
  "Replace line endings into single spaces on the current paragraph."
  (interactive) 
  (let ((fill-column 90002000)) 
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "Replace newline chars in region by single spaces." 
  (interactive "r")
  (let ((fill-column 90002000)) 
    (fill-region start end)))

(defun toggle-letter-case-word-or-region ()
  "Toggle the current word or region's letter case.
Toggles from 3 cases:
upper case, lower case, title case,
in this cyclic order.
Title case means upcase first letter of each word."
(interactive)

(save-excursion
(let (pt pos1 pos2 cap1p cap2p (deactivate-mark nil) (case-fold-search nil))
  (setq pt (point))
  (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning)
            pos2 (region-end))
    (setq pos1 (car (bounds-of-thing-at-point 'word))
          pos2 (cdr (bounds-of-thing-at-point 'word))))

  (goto-char pos1)
  (setq cap1p (looking-at "[A-Z]"))
  (goto-char (1+ pos1))
  (setq cap2p (looking-at "[A-Z]"))

  (cond
   ((and (not cap1p) (not cap2p)) (upcase-initials-region pos1 pos2))
   ((and cap1p (not cap2p)) (upcase-region pos1 pos2) )
   ((and cap1p cap2p) (downcase-region pos1 pos2) )
   (t (downcase-region pos1 pos2) )
   )
  )
)
)

;;; BUFFER RELATED

(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer in cyclic order.\n
Emacs buffers are those starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer in cyclic order.\n
Emacs buffers are those starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun new-empty-buffer ()
  "Opens a new empty buffer."
  (interactive)     
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's 
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.


(defvar recently-closed-buffers (cons nil nil) "A list of 10 last closed buffers.")

(defun close-current-buffer ()
"Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• make sure the buffer shown after closing is a user buffer.
• if the buffer is a file, add the path to the list recently-closed-buffers.

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
 (interactive)
 (let (isEmacsBufferBefore isEmacsBufferAfter)
   (if (string-match "^*" (buffer-name))
       (setq isEmacsBufferBefore t)
     (setq isEmacsBufferBefore nil))

   ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
   (when (and (buffer-modified-p)
              (not isEmacsBufferBefore)
              (not (string-equal mode-name "Dired by name"))
              (not (string-equal mode-name "Dired by date"))
              (not (string-equal "" (save-restriction (widen) (buffer-string)))))
     (if (y-or-n-p
            (concat "Buffer " (buffer-name) " modified; Do you want to save?"))
       (save-buffer)
       (set-buffer-modified-p nil)))

   ;; save to a list of closed buffer
   (when (not (equal buffer-file-name nil))
     (setq recently-closed-buffers
           (cons (cons (buffer-name) (buffer-file-name)) recently-closed-buffers))
     (when (> (length recently-closed-buffers) 10)
           (setq recently-closed-buffers (butlast recently-closed-buffers 1))
           )
     )

   ;; close
   (kill-buffer (current-buffer))

   ;; if emacs buffer, switch
   (if (string-match "^*" (buffer-name))
       (setq isEmacsBufferAfter t)
     (setq isEmacsBufferAfter nil))
   (when isEmacsBufferAfter
     (previous-user-buffer)
     )
   )
 )
