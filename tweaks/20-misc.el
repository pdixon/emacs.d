(show-paren-mode 1)
(setq-default x-stretch-cursor t)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'sentence-end-double-space nil)
(server-start)

(defalias 'list-buffers 'ibuffer)

(require 'mercurial)
(require 'outline-magic)

;; Setup for better printing
(require 'printing)
(pr-update-menus)

;; Setup for ido.el
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Setup for Org
;(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "master.org"))
(setq org-use-fast-todo-selection t)
;; ;; Setup for Org Remember
;; (require 'remember)
;; (org-remember-insinuate)

;; (define-key global-map "\C-cr" 'org-remember)
;; (setq org-remember-templates
;;       '(("Todo" ?t "* TODO %?\n  %i\n  %a" "master.org" "Tasks")
;;         ("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org")
;;         ("Idea" ?i "* %^{Title}\n  %i\n  %a" "master.org" "Ideas")))

;; Setup for xgtags
(require 'xgtags)

;; mail setup
(require 'smtpmail)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil)))
(setq smtpmail-auth-credentials
      '(("smtp.gmail.com"
	 587
	 "phillip.dixon@gmail.com"
	 nil)))
(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)

;; Project Root setup
(require 'project-root)
(setq project-roots
      '(("Hg Hosted Project"
	 :root-contains-files (".hg")
	 :on-hit (lambda (p) (message (car p))))))





;(global-set-key [(control *)] 'my-search-word-forward)


(require 'zenburn)
(unless (zenburn-format-spec-works-p)
  (zenburn-define-format-spec))
(require 'color-theme)
(color-theme-initialize)
; (color-theme-zenburn)

