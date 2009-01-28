(show-paren-mode 1)
(setq-default x-stretch-cursor t)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'sentence-end-double-space nil)
(server-start)

(defalias 'list-buffers 'ibuffer)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
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

;; Setup for eshell mode

;; Setup for yasnippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/snippets/")

(add-hook 'org-mode-hook
          '(lambda ()
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])))

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

(defun anything-project-root-find-files (pattern)
  (when anything-project-root
      (start-process-shell-command "project-root-find"
                                   nil
                                   "find"
                                   anything-project-root
                                   (find-to-string
                                    `(and (prune (name "*.svn" "*.git" "*.hg"))
                                          (name ,(concat "*" pattern "*"))
                                          (type "f"))))))

(defvar anything-c-source-project-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details (project-root-fetch))
              (setq anything-project-root (cdr project-details))))
    (candidates . (lambda ()
                    (anything-project-root-find-files anything-pattern)))
    (type . file)
    (requires-pattern . 2)
    (volatile)
    (delayed)))

(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))


(require 'anything)
(require 'anything-config)
(require 'anything-make)
(setq anything-sources
      (list anything-c-source-buffers
	   ; anything-c-source-filename-history
	   ; anything-c-source-file-cache
	    anything-c-source-make-targets
	    anything-c-source-project-files
	    anything-c-source-occur
	    anything-c-source-info-pages
	    anything-c-source-man-pages
	    anything-c-source-emacs-commands))



;; Customisation for text mode.
(add-hook 'text-mode-hook
	  '(lambda ()
		  (auto-fill-mode 1)
		  (flyspell-mode 1)))

;(global-set-key [(control *)] 'my-search-word-forward)


(require 'zenburn)
(unless (zenburn-format-spec-works-p)
  (zenburn-define-format-spec))
(require 'color-theme)
(color-theme-initialize)
(color-theme-zenburn)

