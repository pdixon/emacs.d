;; Customisation for text mode.
(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode 1)))

(use-package deft
  :bind ("<f5>" . deft)
  :config
  (progn
    (setq deft-extension "org")
    (setq deft-directory "~/personal/notes")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title t)))

(use-package markdown-mode
  :load-path "vendor/markdown-mode/"
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.mdwn\\'" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  (progn
    (setq markdown-command "pandoc")

    (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)))


(use-package pd-blog-helpers
  :commands (pd-blog-draft
             pd-blog-publish-post))

(use-package writegood-mode
  :commands (writegood-mode))

(use-package wc-mode
  :commands (wc-mode))

(provide '40text-mode)
