(show-paren-mode 1)
(setq-default x-stretch-cursor t)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'sentence-end-double-space nil)
(setq inhibit-startup-message t)
(server-start)
(customize-set-variable 'indent-tabs-mode nil)

(setq user-mail-address "phil@dixon.gen.nz")
(setq user-full-name "Phillip Dixon")

(defalias 'list-buffers 'ibuffer)

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
(autopair-global-mode)

;; Setup for better printing
(require 'printing)
(pr-update-menus)

;; Setup for ido.el
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-filename-at-point t)

;; Setup for xgtags
(require 'xgtags)

;; mail setup
;; (require 'smtpmail)
;; (setq starttls-use-gnutls t)
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-service 587)
;; (setq smtpmail-starttls-credentials
;;       '(("smtp.gmail.com" 587 "phil@dixon.gen.nz" nil)))
;; (setq smtpmail-auth-credentials
;;       '(("smtp.gmail.com"
;; 	 587
;; 	 "phil@dixon.gen.nz"
;; 	 nil)))
;; (setq smtpmail-debug-info t)
;; (setq sendmail-program "msmtp")
;; (setq  message-sendmail-extra-arguments '("-a" "dixon.gen.nz"))


(setq message-send-mail-function 'sendmail-send-it)
(setq send-mail-function 'sendmail-send-it)

;; Project Root setup
(require 'project-root)
(setq project-roots
      '(("Xcode Project"
	 :root-contains-files ("*.xcodeproj")
	 :on-hit (lambda (p) (message (car p))))))

(require 'zenburn)
(unless (zenburn-format-spec-works-p)
  (zenburn-define-format-spec))
(require 'color-theme)
(color-theme-initialize)
; (color-theme-zenburn)

