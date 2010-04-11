(require 'nnir)

(setq mail-sources nil)
(setq gnus-auto-subscribed-groups "^nnimap//")
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; I'm not using gnus as a news reader so I don't need any news stuff.
(setq gnus-nntp-server nil
      gnus-read-active-file nil
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-check-new-newsgroups nil)

(setq gnus-select-method '(nnimap "dixon.gen.nz"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)
                                  (user "phil@dixon.gen.nz")
                                  (nnir-search-engine imap)))

;; (setq imap-log t)
(setq gnus-permanently-visible-groups "\.*")

;; Don't automatically make everything I've read expirable.
(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
(add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read)

(gnus-add-configuration
 '(article
   (horizontal 1.0
	       (vertical 30
			 (group 1.0))
	       (vertical 1.0
			 (summary 0.25 point)
			 (article 1.0)))))
(gnus-add-configuration
 '(summary
   (horizontal 1.0
	       (vertical 30
			 (group 1.0))
	       (vertical 1.0
			 (summary 1.0 point)))))

