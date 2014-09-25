(require 'nnir)

(setq mail-sources nil)
(setq gnus-auto-subscribed-groups "^nnimap//")
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
(setq gnus-message-archive-group nil) ; Don't save stuff I send
(setq gnus-treat-display-smileys nil)                                      

(setq gnus-select-method
      '(nnimap "dixon.gen.nz"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (user "phil@dixon.gen.nz")
               (nnir-search-engine imap)))

(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gmane.org"
                    (nntp-connection-timeout 360)
                    (nntp-open-connection-function nntp-open-network-stream)
                    (nntp-address "news.gmane.org")))

;; (add-to-list 'gnus-secondary-select-methods
;;              '(nntp "news.gwene.org"
;;                     (nntp-connection-timeout 360)
;;                     (nntp-open-connection-function nntp-open-network-stream)
;;                     (nntp-address "news.gwene.org")))


;; (setq imap-log t)
(setq gnus-permanently-visible-groups "\.*")
(setq gnus-topic-indent-level 3)
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
;;(setq gnus-group-line-format "%P|%B|%M%o%S%L[%6t|%3i]%6y :%(%~(pad-right 65)g%):%6,6~(cut 2)d\n")

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
