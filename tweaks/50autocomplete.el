(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start t)
(setq ac-dwim t)
(setq ac-override-local-map nil)

(setq-default ac-sources 
              '(ac-source-yasnippet 
                ac-source-abbrev 
                ac-source-words-in-buffer))
