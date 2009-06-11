;; Setup for Org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-directory "~/org/")

(setq org-default-notes-file (concat org-directory "master.org"))
(setq org-agenda-files (list org-directory (concat org-directory "projects/")))

(setq org-hide-leading-stars t)

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@)" "|" "DONE(d!)")
	(sequence "|" "CANCELED")))
(setq org-log-into-drawer "LOGBOOK")
(setq org-tag-alist
      '((:startgroup . nil)
	("@call" . ?c) ("@office" . ?o) ("@home" . ?h) ("@computer" . ?m)
	(:endgroup . nil)))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


(setq org-agenda-custom-commands
      '(("w" "Week's Agenda and Tasks"
	 ((agenda "")
	  (todo "TODO")
	  (todo "WAITING")))))
	  