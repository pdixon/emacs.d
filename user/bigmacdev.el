(setq org-latex-to-pdf-process
      '("/usr/texbin/pdflatex -interaction nonstopmode %s"
        "/usr/texbin/pdflatex -interaction nonstopmode %s"))

(dir-locals-set-class-variables
 'work-directory
 '((nil . ((user-company . "Dynamic Controls")
           (user-mail-address . "pdixon@dynamiccontrols.com")))))

(dir-locals-set-directory-class
 (expand-file-name "~/Documents/work/") 'work-directory)

