(setq mac-option-modifier 'hyper)
(setq mac-command-modifier 'meta)
(setq ispell-program-name "/usr/local/bin/aspell")
(setq org-latex-to-pdf-process
  '("/usr/texbin/pdflatex -interaction nonstopmode %s"
    "/usr/texbin/pdflatex -interaction nonstopmode %s"))