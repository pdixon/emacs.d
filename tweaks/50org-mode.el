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

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d!)")
	(sequence "|" "CANCELED" "DELEGATED(e@)")))
(setq org-log-into-drawer "LOGBOOK")
(setq org-tag-alist
      '((:startgroup . nil)
	("@call" . ?c) ("@office" . ?o) ("@home" . ?h) ("@computer" . ?m) ("@shops" . ?s)
	(:endgroup . nil)
        ("REFILE" . ?r)
        ("PROJECT" . ?p)))
(setq org-use-tag-inheritance nil)

(setq org-stuck-projects
           '("+PROJECT/-CANCELED-DONE" ("TODO")))

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

;; Setup for Org Remember  
(require 'org-protocol)
(require 'remember)
(org-remember-insinuate)

;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

(add-to-list 'org-remember-templates 
	     '("Todo" ?t "* TODO %?\n%U\n%i\n%a" nil bottom nil))
(add-to-list 'org-remember-templates
             '("Notes" ?n "* %?\n%U\n%i\n%a" nil bottom nil))
(add-to-list 'org-remember-templates
             '("Interuption" ?i "\n* %?\n :CLOCK-IN: \n" nil bottom nil))

;; Refile setup
(setq org-completion-use-ido t)
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3))))
(setq org-refile-use-outline-path (quote file))
(setq org-outline-path-complete-in-steps t)

;; MobileOrg Support
(setq org-mobile-directory "/Volumes/pdixon/org")
(setq org-mobile-inbox-for-pull "~/org/inbox.org")

;; org-track
(require 'org-install)
(require 'org-track)
(setq org-track-directory (concat dotfiles-dir "/vendor"))

;; LaTeX Class setup.
(eval-after-load "org-latex" 
  '(add-to-list 'org-export-latex-classes
                ;; beamer class for presentations.
                '("beamer"
                  "\\documentclass[11pt]{beamer}\n
                \\mode<{{{beamermode}}}>\n
                \\usetheme{{{{beamertheme}}}}\n
                \\usecolortheme{{{{beamercolortheme}}}}\n
                \\beamertemplateballitem\n
                \\setbeameroption{show notes}\n
                \\usepackage[utf8]{inputenc}\n
                \\usepackage{hyperref}\n
                \\usepackage{color}\n
                \\usepackage{verbatim}\n
                \\institute{{{{beamerinstitute}}}}\n
                \\subject{{{{beamersubject}}}}\n"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\begin{frame}[fragile]\\frametitle{%s}"
                   "\\end{frame}"
                   "\\begin{frame}[fragile]\\frametitle{%s}"
                   "\\end{frame}"))))

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
          (lambda ()
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)))