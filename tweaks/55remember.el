;; Setup for Org Remember
(require 'org-install)
(require 'remember)
(org-remember-insinuate)
(require 'org-mac-protocol)

;; (setq remember-annotation-functions '(org-remember-annotation))
;; (setq remember-handler-functions '(org-remember-handler))
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)

(add-to-list 'org-remember-templates 
	     '("Todo" ?t "* TODO %?\n  %i\n  %a" "master.org" "Tasks"))
(add-to-list 'org-remember-templates
        '("Idea" ?i "* %^{Title}\n  %i\n  %a" "master.org" "Ideas"))
(add-to-list 'org-remember-templates
	'("Notes" ?n "* %^{Title}\n  %i\n  %a" "master.org" "Notes"))
(add-to-list 'org-remember-templates
	'("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org"))


;; (?w "* %^{Title}\n\n  Source: %u, %c\n\n  %i" "master.org" "Notes")
