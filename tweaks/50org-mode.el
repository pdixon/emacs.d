;; Setup for Org
(use-package org-agenda
  :bind (("<f6>" . org-agenda)
         ("C-c a" . org-agenda)))

(use-package org-capture
  :bind (("C-c r" . org-capture)))

(use-package ox-html
  :defer t
  :config
  (progn
    (require 'htmlize)
    (setq org-html-htmlize-output-type 'css)))

(use-package ox-publish
  :defer t
  :commands pd/publish-blog
  :config
  (progn
    (require 'ox-html)
    (require 'pd-html)

    (defun pd/publish-blog ()
      "Publish my blog"
      (interactive)
      (org-publish-remove-all-timestamps)
      (org-publish-project "blog"))

    (setq org-publish-project-alist
          '(("blog-posts"
             :base-directory "~/personal/phil.dixon.gen.nz/posts"
             :base-extension "org"
             :publishing-directory "~/Sites/phil.dixon.gen.nz/posts"
             :publishing-function pd-html-publish-to-html
             :with-toc nil
             :section-numbers nil
             :auto-sitemap t
             :sitemap-title ""
             :sitemap-filename "index.org"
             :sitemap-sort-files anti-chronologically
             :sitemap-file-entry-format "%t (%d)")
            ("blog-drafts"
             :base-directory "~/personal/phil.dixon.gen.nz/drafts"
             :base-extension "org"
             :publishing-directory "~/Sites/phil.dixon.gen.nz/drafts"
             :publishing-function pd-html-publish-to-html
             :with-toc nil
             :section-numbers nil
             :auto-sitemap t
             :sitemap-title ""
             :sitemap-filename "index.org"
             :sitemap-sort-files anti-chronologically
             :sitemap-file-entry-format "%t (%d)")
            ("blog-pages"
             :base-directory "~/personal/phil.dixon.gen.nz/"
             :base-extension "org"
             :publishing-directory "~/Sites/phil.dixon.gen.nz/"
             :publishing-function pd-html-publish-to-html
             :with-toc nil
             :section-numbers nil
             :creator-info nil)
            ("blog-static"
             :base-directory "~/personal/phil.dixon.gen.nz/"
             :base-extension "jpg\\|png\\|css\\|js"
             :recursive t
             :publishing-directory "~/Sites/phil.dixon.gen.nz/"
             :publishing-function org-publish-attachment)
            ("blog"
             :components
             ("blog-pages" "blog-posts" "blog-drafts" "blog-static"))))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (progn
            (setq org-directory "~/org/")
            (setq org-default-notes-file (concat org-directory "inbox.org"))
            (setq org-agenda-diary-file (concat org-directory "diary.org"))
            (setq org-agenda-files (list org-directory (concat org-directory "projects/")))

            (setq org-hide-leading-stars t)
            (setq org-use-sub-superscripts "{}")

            (setq org-fast-tag-selection-single-key 'expert)
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


            (setq org-use-fast-todo-selection t)
            (setq org-todo-keywords
                  '((sequence "TODO(t)" "|" "DONE(d!)")
                    (sequence "WAITING(w@/!)" "|" "CANCELLED" "DELEGATED(e@)")))
            (setq org-enforce-todo-depedencies t)
            (defun pd/org-summary-todo (n-done n-not-done)
              "Switch entry to DONE when all subentries are done, to TODO otherwise."
              (let (org-log-done org-log-states)   ; turn off logging
                (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
            (add-hook 'org-after-todo-statistics-hook 'pd/org-summary-todo)

            (setq org-agenda-todo-ignore-with-date t)
            (setq org-agenda-skip-deadline-if-done t)
            (setq org-agenda-skip-scheduled-if-done t)
            (setq org-agenda-tags-todo-honor-ignore-options t)
            (setq org-agenda-window-setup 'current-window)
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
                                  (org-agenda-skip-function 'pd/skip-projects)
                                  (org-agenda-todo-ignore-scheduled t)
                                  (org-agenda-todo-ignore-deadlines t)))
                      (tags-todo "-SOMEDAY/!-WAITING"
                                 ((org-agenda-overriding-header "Tasks")
                                  (org-agenda-skip-function 'pd/skip-projects)
                                  (org-agenda-todo-ignore-scheduled t)
                                  (org-agenda-todo-ignore-deadlines t)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      nil))
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
                      (org-agenda-skip-function 'pd/skip-projects)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-todo-ignore-deadlines 'future)))
                    ("e" "Errand List" tags-todo "@shops"
                     ((org-agenda-prefix-format "[ ]")
                      (org-agenda-todo-keyword-format "")))
                    ("c" todo "TODO"
                     ((org-agenda-sorting-strategy '(tag-up priority-down))))))

            (add-hook 'org-agenda-mode-hook '(lambda ()
                                               (setq org-agenda-tags-column (- (window-width)))))


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

            (defun bh/list-sublevels-for-projects-indented ()
              "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
              (if (marker-buffer org-agenda-restrict-begin)
                  (setq org-tags-match-list-sublevels 'indented)
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

            (defun pd/skip-projects ()
              "Skip trees that are projects"
              (save-restriction
                (widen)
                (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
                  (cond
                   ((bh/is-project-p)
                    next-headline)
                   (t
                    nil)))))))

(provide '50org-mode)
