
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

(setq markdown-command "pandoc")

(defun markdown-yas-fixup ()
  (local-set-key "\t" 'markdown-cycle)
  (local-set-key [tab] 'yas/expand))

(defun markdown-imenu-setup ()
  (setq imenu-generic-expression '(("Sections" "^#+ .+" 0)
                                   ("Refs" "^\\[\\(.+\\)\\]:" 1))))

(add-hook 'markdown-mode-hook 'markdown-yas-fixup)
(add-hook 'markdown-mode-hook 'markdown-imenu-setup)
