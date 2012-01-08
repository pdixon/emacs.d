;; (setq mac-option-modifier 'hyper)
;; (setq mac-command-modifier 'meta)
(setq ispell-program-name "/opt/homebrew/bin/aspell")

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
      '(("iChair Trac" "http://ichair.sw/report/7?format=rss&USER=pdixon"
         "~/org/inbox.org" "iChair Assigned Issues")))

(setq url-proxy-services '(("no_proxy" . "\\.sw")
			   ("http" . "127.0.0.1:3128")))

(when window-system
  (ubuntu-mono 1))
