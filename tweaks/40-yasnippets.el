;; Setup for yasnippets
(require 'yasnippet)
(yas/initialize)

(setq yas/root-directory '((concat dotfiles-dir "mysnippets/")
                           (concat dotfiles-dir "vendor/yasnippets/snippets/")))

(yas/load-directory yas/root-directory)

(add-hook 'org-mode-hook
          (let ((original-command (lookup-key org-mode-map [tab])))
            `(lambda ()
               (setq yasl/fallback-behaviour
                     '(apply 'original-command))
               (local-set-key [tab] 'yas/expand))))
