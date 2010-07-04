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

(show-paren-mode 1)    
(setq-default x-stretch-cursor t)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'sentence-end-double-space nil)
(setq inhibit-startup-message t)
(server-start)
(customize-set-variable 'indent-tabs-mode nil)
(global-auto-revert-mode 1)


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
(setq message-kill-buffer-on-exit t)

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


;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Transparently open compressed files
(auto-compression-mode t)
 
;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)
 
;; Save a list of recent files visited.
(recentf-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(require 'autopair)
;(autopair-global-mode)

;; Setup for better printing
(require 'printing)
(pr-update-menus)

;; Setup for ido.el
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point t)

;; Setup for xgtags
;; (require 'xgtags)

(require 'zenburn)
(unless (zenburn-format-spec-works-p)
  (zenburn-define-format-spec))
(require 'color-theme)
(color-theme-initialize)
; (color-theme-zenburn)

(provide '20-misc)
;;; 20-misc.el ends here

