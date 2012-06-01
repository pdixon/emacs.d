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
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))


(setq-default x-stretch-cursor t)
;; (setq-default cursor-type 'bar)
(setq inhibit-startup-message t)
(setq vc-handled-backends nil)
(setq whitespace-style '(face trailing tabs)
      whitespace-line-column 80)
(setq message-kill-buffer-on-exit t)
(setq diff-switches "-u")


(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'sentence-end-double-space nil)

(server-start)
(customize-set-variable 'indent-tabs-mode nil)
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "phil@dixon.gen.nz")
(setq user-full-name "Phillip Dixon")

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

(defalias 'list-buffers 'ibuffer)
(setq ibuffer-expert 1)
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("org" (or (mode . org-mode)
                          (name . "^\\*Org Agenda\\*$")))
               (".emacs" (filename . ".emacs.d/"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(eval-after-load 'ibuffer
  '(progn
     ;; Use human readable Size column instead of original one
     (define-ibuffer-column size-h
       (:name "Size" :inline t)
       (cond
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        (t (format "%8d" (buffer-size)))))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(show-paren-mode 1)

;; Transparently open compressed files
(auto-compression-mode t)
 
;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)
 
;; Save a list of recent files visited.
(recentf-mode 1)

(global-auto-revert-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)
(electric-layout-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Setup for better printing
(require 'printing)
(pr-update-menus)

;; Setup for ido.el
(when (functionp 'ido-mode)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-auto-merge-work-directories-length nil
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t
        ido-handle-duplicate-virtual-buffers 2
        ido-max-prospects 10))

;; auto-complete in minibuffer
(icomplete-mode 1)

(savehist-mode t)

(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)

;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))

;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

(load-theme 'solarized-light t t)
(load-theme 'solarized-dark t t)

(defun pd/light ()
  "Activate light theme."
  (interactive)
  (custom-set-variables '(custom-enabled-themes '(solarized-light))))

(defun pd/dark ()
  "Activate dark theme."
  (interactive)
  (custom-set-variables '(custom-enabled-themes '(solarized-dark))))


;; make scripts executable on save.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

(provide '20-misc)
;;; 20-misc.el ends here
