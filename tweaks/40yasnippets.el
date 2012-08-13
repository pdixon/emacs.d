;; Setup for yasnippets

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(use-package yasnippet
  :commands (yas/minor-mode yas/expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (hook-into-modes #'(lambda () (yas/minor-mode 1))
                   '(prog-mode-hook
                     org-mode-hook
                     message-mode-hook
                     markdown-mode-hook))
  :config
  (progn
    (setq yas/root-directory (concat dotfiles-dir "snippets"))
    (yas/load-directory yas/root-directory)))
