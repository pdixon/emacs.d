;;; 20-misc.el --- Miscellanous setup

;; Copyright (C) 2010 Phillip Dixon

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

(when window-system
  (setq frame-title-format '(buffer-file-name "emacs - %f" ("emacs - %b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Apperance
(setq solarized-distinct-fringe-background t
      solarized-use-variable-pitch nil
      solarized-height-minus-1 1.0
      solarized-height-plus-1 1.0
      solarized-height-plus-2 1.0
      solarized-height-plus-3 1.0
      solarized-height-plus-4 1.0)

(setq vc-handled-backends '(Git Hg))
(setq whitespace-style '(face trailing tabs)
      whitespace-line-column 80)


;;(server-start)
(setq split-height-threshold nil
      split-width-threshold nil)

(setq message-kill-buffer-on-exit t)
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "phil@dixon.gen.nz")
(setq user-full-name "Phillip Dixon")

(eudc-set-server "localhost" 'mab t)
(eudc-protocol-set 'eudc-inline-expansion-format
                   '("%s %s <%s>" firstname lastname email)
                   'mab)

(defun eudc-select (choices beg end)
    (let ((replacement
           (ido-completing-read "Multiple matches found; choose one: "
                                (mapcar 'list choices))))
      (delete-region beg end)
      (insert replacement)))

(setq send-mail-function 'smtpmail-send-it 
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "phil@dixon.gen.nz" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq ispell-dictionary "en_GB-ise"
      ispell-extra-args `("--keyboard=dvorak"))

(use-package imenu
  :defer t
  :config
  (progn
    (setq imenu-max-items 200)))

(use-package ibuffer
  :defer t
  :bind ("<f8>" . ibuffer)
  :init
  (progn
    (defalias 'list-buffers 'ibuffer))
  :config
  (progn
    (setq ibuffer-expert 1)
    (setq ibuffer-show-empty-filter-groups nil)

    (defun pd/dired-do-multi-occur (regexp)
      "Show all in lines in marked files containing REGEXP"
      (interactive "MList lines matching regexp: ")
      (multi-occur (mapcar 'find-file (dired-get-marked-files)) regexp))

    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))

    ;; Use human readable Size column instead of original one
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
       ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
       (t (format "%8d" (buffer-size)))))

    ;; Modify the default ibuffer-formats
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 18 18 :left :elide)
                  " "
                  (size-h 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process)))))


(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse
          uniquify-separator "/"
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))

(use-package dired
  :defer t
  :config
  (progn
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-dwim-target t
          dired-recursive-copies 'always
          dired-recursive-deletes 'top)
    (defun pd-dired-find-alternate-parent ()
      (interactive)
      (find-alternate-file ".."))
    (bind-key "^" 'pd-dired-find-alternate-parent dired-mode-map)))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(defun pd/light ()
  "Activate light theme."
  (interactive)
  (load-theme 'solarized-light t t)
  (load-theme 'pd-basic t t)
  (custom-set-variables '(custom-enabled-themes '(pd-basic solarized-light))))

(defun pd/dark ()
  "Activate dark theme."
  (interactive)
  (load-theme 'solarized-dark t t)
  (load-theme 'pd-basic t t)
  (custom-set-variables '(custom-enabled-themes '(pd-basic solarized-dark))))

(defun pd/zenburn ()
  "Activate dark theme."
  (interactive)
  (load-theme 'zenburn t t)
  (load-theme 'pd-basic t t)
  (custom-set-variables '(custom-enabled-themes '(pd-basic zenburn))))


;; make scripts executable on save.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package expand-region
  :bind (("M-\"" . er/contract-region)
         ("M-'" . er/expand-region)))

(use-package magit
  :bind ("<f7>" . magit-status)
  :config
  (progn
    (setq magit-status-buffer-switch-function 'switch-to-buffer
          magit-diff-refine-hunk t)
    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))))

(use-package hg-commit-mode
  :mode ("hg-editor-.*\\.txt\\'" . hg-commit-mode))

(use-package ediff
  :defer t
  :config
  (progn
    (setq ediff-split-window-function 'split-window-horizontally)))

(use-package pd-editing-extras
  :bind (("C-c +" . my-increment)
         ("C-t" . transpose-dwim)
         ("M-c". toggle-letter-case)
         ("M-<SPC>" . cycle-spacing)))

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-J" 'delete-indentation-forward)
(bind-key "M-j" 'delete-indentation)

(bind-key "C-h a" 'apropos)

(bind-key "C-c y" 'bury-buffer)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(use-package bookmark
  :bind ("<f9>" . bookmark-bmenu-list))

(use-package multiple-cursors
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)))

;; Yank line or region
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

;; Kill line or region
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(use-package hexl-mode
  :mode (("\\.exe\\'" . hexl-mode)
         ("\\.dll\\'" . hexl-mode)))

(use-package find-file-in-project
  :bind ("C-c C-f" . ffip)
  :config
  (progn
    (require 'pd-project)
    (setq ffip-project-root-function 'pd-project-get-root)))

(use-package pd-project
  :bind(("C-c b" . pd-project-compile))
  :commands (pd-project-grep
             pd-project-todo))

(use-package pd-window-extras
  :commands (pd/rotate-windows
             pd/toggle-window-split
             pd/setup-windows
             pd/toggle-just-one-window))

(provide '20misc)
;;; 20-misc.el ends here
