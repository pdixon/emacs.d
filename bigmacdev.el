(setq org-latex-to-pdf-process
      '("/usr/texbin/pdflatex -interaction nonstopmode %s"
        "/usr/texbin/pdflatex -interaction nonstopmode %s"))

(dir-locals-set-class-variables
 'work-directory
 '((nil . ((user-company . "Dynamic Controls")
           (user-mail-address . "pdixon@dynamiccontrols.com")))))

(dir-locals-set-directory-class
 (expand-file-name "~/Documents/work/") 'work-directory)

(setq org-feed-alist
      '(("UniPD Trac Tickets" "http://tracunipd.au.ivc/report/7?format=rss&USER=pdixon"
         "~/org/inbox.org" "UniPD Assigned Issues")
        ("UniShark Trac Tickets" "http://tracunishark.au.ivc/report/7?format=rss&USER=pdixon"
         "~/org/inbox.org" "UniShark Assigned Issues")))

(setq url-proxy-services '(("no_proxy" . "\\.au.ivc")
                           ("http" . "127.0.0.1:3128")))
(require 'mediawiki)
(add-to-list 'mediawiki-site-alist
             '("Software" "http://wiki.sw.au.ivc/mediawiki" "pdixon" "" "The PENSIEVE"))

(setq mediawiki-site-default "Software")
