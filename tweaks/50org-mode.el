;; Setup for Org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-directory "~/org/")

(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-agenda-files (list org-directory (concat org-directory "projects/")))

(setq org-hide-leading-stars t)

;; Keep tasks with dates off the global todo lists
(setq org-agenda-todo-ignore-with-date t)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d!)")
	(sequence "|" "CANCELED" "DELEGATED(e@)")))
(setq org-log-into-drawer "LOGBOOK")
(setq org-tag-alist
      '((:startgroup . nil)
	("@call" . ?c) ("@office" . ?o) ("@home" . ?h) ("@computer" . ?m)
	(:endgroup . nil)
        ("REFILE" . ?r)))

(setq org-enforce-todo-depedencies t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


(setq org-agenda-custom-commands
      '(("w" "Week's Agenda and Tasks"
	 ((agenda)
	  (todo "TODO")
	  (todo "WAITING"))
         nil
         ("~/org/output/weekly.org"))))
	  
(require 'org-protocol)

;; Refile setup
(setq org-completion-use-ido t)
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3))))
(setq org-refile-use-outline-path (quote file))
(setq org-outline-path-complete-in-steps t)

;; Babel mode
(require 'org-babel-init)
(require 'org-babel-python)

(org-babel-load-library-of-babel)

;; MobileOrg Support
(setq org-mobile-directory "/Volumes/pjdixon/org")
(setq org-mobile-inbox-for-pull "~/org/inbox.org")

;; Website and Blog setup
(require 'org-blog)
(setq org-blog-directory "~/website/blog/")
(setq org-blog-directory "~/website/blog/unfinished/")

(setq org-publish-project-alist
      '(("blog"
	 :base-directory "~/website/blog/"
         :base-extension "org"
	 :publishing-directory "~/Sites/blog/"
	 :publishing-function org-publish-org-to-html
	 :auto-index t
	 :blog-base-url "http://localhost/~pjd67/blog/"
	 :blog-title "e?macs"
	 :blog-description "Phil Dixon on emacs and macs"
	 :blog-export-rss nil
	 :index-function org-publish-blog-index
	 :index-filename "index.org"
	 :index-title "Clever Title Here"
	 :index-posts 2
	 :auto-preamble t
	 :auto-postamble t)

	("static"
	 :bash-directory "~/website/static"
	 :base-extension "css\\|js\\|png\\|jpg\\|pdf"
	 :publishing-directory "~/Sites/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )

	("web"
	 :bash-directory "~/website/"
	 :base-extension "org"
	 :publishing-directory "~/Sites/"
	 :recursive t
	 :publishing-function org-publish-org-to-html
	 :auto-preamble t
	 )))


