;; Setup for Org Remember
(require 'remember)
(org-remember-insinuate)

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "master.org" "Tasks")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "master.org" "Ideas")
	("Notes" ?n "* %^{Title}\n  %i\n  %a" "master.org" "Notes")
	("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org")))
