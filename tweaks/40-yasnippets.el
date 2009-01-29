;; Setup for yasnippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "snippets/"))

(add-hook 'org-mode-hook
          '(lambda ()
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])))
