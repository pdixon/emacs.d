(setq user-mail-address "phillip.dixon@gmail.com")
(setq user-full-name "Phillip Dixon")
(setq gnus-invalid-group-regexp "")
;; (setq mail-sources nil)

;; I'm not using gnus as a news reader so I don't need any news stuff.
(setq gnus-nntp-server nil
      gnus-read-active-file nil
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-check-new-newsgroups nil)

;; (setq gnus-select-method '(nnnil ""))
;; (setq gnus-select-methods '((nnimap "gmail"
;; 				    (nnimap-address "imap.gmail.com")
;; 				    (nnimap-server-port 993)
;; 				    (nnimap-stream ssl))))
(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)))

(setq imap-log t)