;; Setup for yasnippets

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory (concat dotfiles-dir "mysnippets"))
(yas/load-directory yas/root-directory)
