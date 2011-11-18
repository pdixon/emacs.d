;; Setup for Org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-directory "~/org/")

(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-agenda-diary-file (concat org-directory "diary.org"))
(setq org-agenda-files (list org-directory (concat org-directory "projects/")))

(setq org-hide-leading-stars t)

;; Keep tasks with dates off the global todo lists
(setq org-agenda-todo-ignore-with-date t)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!)")
	(sequence "WAITING(w@)" "|" "CANCELED" "DELEGATED(e@)")))
(setq org-log-into-drawer "LOGBOOK")
(setq org-tag-alist
      '((:startgroup . nil)
	("@call" . ?c) ("@office" . ?o) ("@home" . ?h) ("@read" . ?r) ("@computer" . ?m) ("@shops" . ?s) ("@dev" . ?d) ("@write" . ?w)
	(:endgroup . nil)
        ("REFILE" . ?f)
        ("PROJECT" . ?p)
        ("SOMEDAY" . ?s)))
(setq org-use-tag-inheritance t)
(setq org-tags-exclude-from-inheritance '("@call"
                                          "@office"
                                          "@home"
                                          "@read"
                                          "@computer"
                                          "@shops"
                                          "@dev"
                                          "@write"
                                          "PROJECT"))

(setq org-stuck-projects
           '("+PROJECT/-CANCELED-DONE" ("TODO")))

(setq org-enforce-todo-depedencies t)
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Effort estimation.
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "1:00 2:00 4:00 8:00 16:00 40:00"))))

(setq org-agenda-custom-commands
      '(("w" "Week's Agenda and Tasks"
	 ((agenda)
          (tags-todo "+PROJECT-SOMEDAY/!")
	  (tags-todo "-SOMEDAY-PROJECT/!+TODO|+STARTED")
	  (todo "WAITING"))
         nil
         ("~/org/output/weekly.org"))
        ("p" "Project Lists"
         ((tags-todo "+PROJECT-SOMEDAY/!")
          (stuck "")
          (tags "+PROJECT+SOMEDAY")))
        ("e" "Errand List" tags-todo "@shops"
         ((org-agenda-prefix-format "[ ]")
          (org-agenda-todo-keyword-format "")))
        ("c" todo "TODO"
         ((org-agenda-sorting-strategy '(tag-up priority-down))))))

;; Setup for Org Remember
(require 'org-protocol)

;; Org Capture 
(setq org-capture-templates
      '(("i" "Interruption" entry
        (file "~/org/inbox.org")
        "* %?\n"
        :clock-in t)
       ("n" "Notes" entry
        (file "~/org/inbox.org")
        "* %?\n%U\n%i\n%a")
       ("t" "Todo" entry
        (file "~/org/inbox.org")
        "* TODO %?\n%U\n%i\n%a")
       ("w"
         "Default template"
         entry
         (file "~/org/inbox.org")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
         :empty-lines 1)))

;; Refile setup
(setq org-completion-use-ido t)
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3))))
(setq org-refile-use-outline-path (quote file))
(setq org-outline-path-complete-in-steps t)

;; MobileOrg Support
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/inbox.org")

(setq org-publish-project-alist
      '(("static"
	 :base-directory "~/website/static"
	 :base-extension "css\\|js\\|png\\|jpg\\|pdf"
	 :publishing-directory "~/Sites/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )

	("web"
	 :base-directory "~/website/"
	 :base-extension "org"
	 :publishing-directory "~/Sites/"
	 :recursive t
	 :publishing-function org-publish-org-to-html
	 :auto-preamble t
	 )))

(add-hook 'org-mode-hook
          (let ((original-command (lookup-key org-mode-map [tab])))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
               (local-set-key [tab] 'yas/expand)
               (electric-indent-mode -1))))

; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt)

; This is at the end of my .emacs - so appointments are set up when Emacs starts
;;(bh/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)
; Make sure any org buffers get saved, ready for the the auto commit just after the hour.
(run-at-time "00:59" 3600 'org-save-all-org-buffers)
