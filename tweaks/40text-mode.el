;; Customisation for text mode.
(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode 1)))

(use-package deft
  :bind ("<f5>" . deft)
  :config
  (progn
    (setq deft-extension "mdwn")
    (setq deft-directory "~/Dropbox/Elements/")
    (setq deft-text-mode 'markdown-mode)
    (setq deft-use-filename-as-title t)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.mdwn\\'" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  (progn
    (setq markdown-command "pandoc")

    (defun markdown-imenu-setup ()
      (setq imenu-generic-expression '(("Sections" "^#+ .+" 0)
                                       ("Refs" "^\\[\\(.+\\)\\]:" 1))))

    (add-hook 'markdown-mode-hook 'markdown-imenu-setup)))

(use-package pd-blog-helpers
  :commands (pd-blog-draft
             pd-blog-publish-post))

(use-package writegood-mode
  :commands (writegood-mode))

(provide '40text-mode)
