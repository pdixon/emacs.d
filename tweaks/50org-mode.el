;; Setup for Org
(use-package org-capture
  :bind ("C-c r" . org-capture)
  :config (progn
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
                     :empty-lines 1)))))

(use-package org-agenda
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("<f6>" . org-agenda))
  :config (progn
            (defun pd-org-hook ()
              ;;(electric-indent-mode -1)
              ;;(electrict-layout-mode -1)
              )

            (add-hook 'org-mode-hook 'pd-org-hook)
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
          (setq org-agenda-window-setup 'current-window)
          (setq org-use-fast-todo-selection t)
          (setq org-todo-keywords
                '((sequence "TODO(t)" "|" "DONE(d!)")
                  (sequence "WAITING(w@/!)" "|" "CANCELLED" "DELEGATED(e@)")))
          (setq org-log-into-drawer "LOGBOOK")
          (setq org-tag-alist
                '((:startgroup . nil)
                  ("@call" . ?c)
                  ("@office" . ?o)
                  ("@home" . ?h)
                  ("@read" . ?r)
                  ("@computer" . ?m)
                  ("@shops" . ?s)
                  ("@dev" . ?d)
                  ("@write" . ?w)
                  (:endgroup . nil)
                  ("REFILE" . ?f)
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

          (setq org-stuck-projects '("" nil nil ""))

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

          (setq org-agenda-compact-blocks t)
          (setq org-agenda-custom-commands
                '(("w" "Day's Agenda and Tasks"
                   ((agenda "" (( org-agenda-span 1)))
                    (tags-todo "-SOMEDAY/!"
                               ((org-agenda-overriding-header "Stuck Projects")
                                (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                    (tags-todo "-SOMEDAY/!"
                               ((org-agenda-overriding-header "Projects")
                                (org-agenda-skip-function 'bh/skip-non-projects)
                                (org-agenda-ignore-scheduled 'future)
                                (org-agenda-ignore-deadlines 'future)
                                (org-agenda-sorting-strategy
                                 '(category-keep))))
                    (tags-todo "-CANCELLED/!WAITING"
                               ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                (org-agenda-skip-function 'bh/skip-projects-and-habits)
                                (org-agenda-todo-ignore-scheduled t)
                                (org-agenda-todo-ignore-deadlines t)))
                    (tags-todo "-SOMEDAY/!-WAITING"
                               ((org-agenda-overriding-header "Tasks")
                                (org-agenda-skip-function 'pd/skip-projects)
                                (org-agenda-todo-ignore-scheduled t)
                                (org-agenda-todo-ignore-deadlines t)
                                (org-agenda-sorting-strategy
                                 '(category-keep))))
                    nil
                    ("~/org/output/weekly.org")))
                  ("#" "Stuck Projects" tags-todo "-SOMEDAY/!"
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                  ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-WAITING"
                   ((org-agenda-overriding-header "Tasks")
                    (org-agenda-skip-function 'pd/skip-projects)
                    (org-agenda-sorting-strategy
                     '(category-keep))))
                  ("p" "Project Lists" tags-todo "-SOMEDAY/!"
                   ((org-agenda-overriding-header "Projects")
                    (org-agenda-skip-function 'bh/skip-non-projects)
                    (org-agenda-ignore-scheduled 'future)
                    (org-agenda-ignore-deadlines 'future)
                    (org-agenda-sorting-strategy
                     '(category-keep))))
                  ("b" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING"
                   ((org-agenda-overriding-header "Waiting and Postponed tasks")
                    (org-agenda-skip-function 'bh/skip-projects-and-habits)
                    (org-agenda-todo-ignore-scheduled 'future)
                    (org-agenda-todo-ignore-deadlines 'future)))
                  ("e" "Errand List" tags-todo "@shops"
                   ((org-agenda-prefix-format "[ ]")
                    (org-agenda-todo-keyword-format "")))
                  ("c" todo "TODO"
                   ((org-agenda-sorting-strategy '(tag-up priority-down))))))

          (add-hook 'org-agenda-mode-hook '(lambda ()
                                             (setq org-agenda-tags-column (- (window-width)))))

          ;; Org Capture

          ;; Refile setup
          (setq org-completion-use-ido t)
          (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3))))
          (setq org-refile-use-outline-path (quote file))
          (setq org-outline-path-complete-in-steps t)

          (run-at-time "00:59" 3600 'org-save-all-org-buffers)

          (defun bh/is-project-p ()
            "Any task with a todo keyword subtask"
            (save-restriction
              (widen)
              (let ((has-subtask)
                    (subtree-end (save-excursion (org-end-of-subtree t)))
                    (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
                (save-excursion
                  (forward-line 1)
                  (while (and (not has-subtask)
                              (< (point) subtree-end)
                              (re-search-forward "^\*+ " subtree-end t))
                    (when (member (org-get-todo-state) org-todo-keywords-1)
                      (setq has-subtask t))))
                (and is-a-task has-subtask))))

          (defun bh/is-project-subtree-p ()
            "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
            (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                        (point))))
              (save-excursion
                (bh/find-project-task)
                (if (equal (point) task)
                    nil
                  t))))

          (defun bh/is-task-p ()
            "Any task with a todo keyword and no subtask"
            (save-restriction
              (widen)
              (let ((has-subtask)
                    (subtree-end (save-excursion (org-end-of-subtree t)))
                    (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
                (save-excursion
                  (forward-line 1)
                  (while (and (not has-subtask)
                              (< (point) subtree-end)
                              (re-search-forward "^\*+ " subtree-end t))
                    (when (member (org-get-todo-state) org-todo-keywords-1)
                      (setq has-subtask t))))
                (and is-a-task (not has-subtask)))))

          (defun bh/is-subproject-p ()
            "Any task which is a subtask of another project"
            (let ((is-subproject)
                  (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
              (save-excursion
                (while (and (not is-subproject) (org-up-heading-safe))
                  (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                    (setq is-subproject t))))
              (and is-a-task is-subproject)))

          (defun bh/list-sublevels-for-projects-indented ()
            "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
            (if (marker-buffer org-agenda-restrict-begin)
                (setq org-tags-match-list-sublevels 'indented)
              (setq org-tags-match-list-sublevels nil))
            nil)

          (defun bh/list-sublevels-for-projects ()
            "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
            (if (marker-buffer org-agenda-restrict-begin)
                (setq org-tags-match-list-sublevels t)
              (setq org-tags-match-list-sublevels nil))
            nil)

          (defun bh/skip-non-stuck-projects ()
            "Skip trees that are not stuck projects"
            (save-restriction
              (widen)
              (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                (if (bh/is-project-p)
                    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                           (has-next (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward "^\\*+ \\(TODO\\) " subtree-end t)))))
                      (if has-next
                          next-headline
                        nil)) ; a stuck project, has subtasks but no next task
                  next-headline))))

          (defun bh/skip-non-projects ()
            "Skip trees that are not projects"
            (bh/list-sublevels-for-projects-indented)
            (if (save-excursion (bh/skip-non-stuck-projects))
                (save-restriction
                  (widen)
                  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                    (if (bh/is-project-p)
                        nil
                      subtree-end)))
              (org-end-of-subtree t)))

          (defun bh/skip-project-trees-and-habits ()
            "Skip trees that are projects"
            (save-restriction
              (widen)
              (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                (cond
                 ((bh/is-project-p)
                  subtree-end)
                 ((org-is-habit-p)
                  subtree-end)
                 (t
                  nil)))))

          (defun pd/skip-projects ()
            "Skip trees that are projects"
            (save-restriction
              (widen)
              (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                (cond
                 ((bh/is-project-p)
                  next-headline)
                 (t
                  nil)))))

          (defun bh/skip-projects-and-habits-and-single-tasks ()
            "Skip trees that are projects, tasks that are habits, single non-project tasks"
            (save-restriction
              (widen)
              (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                (cond
                 ((org-is-habit-p)
                  next-headline)
                 ((bh/is-project-p)
                  next-headline)
                 ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
                  next-headline)
                 (t
                  nil)))))

          (defun bh/skip-project-tasks-maybe ()
            "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
            (save-restriction
              (widen)
              (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                     (next-headline (save-excursion (or (outline-next-heading) (point-max))))
                     (limit-to-project (marker-buffer org-agenda-restrict-begin)))
                (cond
                 ((bh/is-project-p)
                  next-headline)
                 ((org-is-habit-p)
                  subtree-end)
                 ((and (not limit-to-project)
                       (bh/is-project-subtree-p))
                  subtree-end)
                 ((and limit-to-project
                       (bh/is-project-subtree-p)
                       (member (org-get-todo-state) (list "NEXT")))
                  subtree-end)
                 (t
                  nil)))))

          (defun bh/skip-projects-and-habits ()
            "Skip trees that are projects and tasks that are habits"
            (save-restriction
              (widen)
              (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                (cond
                 ((bh/is-project-p)
                  subtree-end)
                 ((org-is-habit-p)
                  subtree-end)
                 (t
                  nil)))))

          (defun bh/skip-non-subprojects ()
            "Skip trees that are not projects"
            (let ((next-headline (save-excursion (outline-next-heading))))
              (if (bh/is-subproject-p)
                  nil
                next-headline)))

          (defun bh/find-project-task ()
            "Move point to the parent (project) task if any"
            (save-restriction
              (widen)
              (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
                (while (org-up-heading-safe)
                  (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                    (setq parent-task (point))))
                (goto-char parent-task)
                parent-task)))))

(provide '50org-mode)
