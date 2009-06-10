;; Setup for Org Remember
(require 'remember)

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "master.org" "Tasks")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "master.org" "Ideas")
	("Notes" ?n "* %^{Title}\n  %i\n  %a" "master.org" "Notes")
	("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org")
	(?w "* %^{Title}\n\n  Source: %u, %c\n\n  %i" "master.org" "Notes")))
