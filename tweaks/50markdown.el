
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

(defun markdown-yas-fixup ()
  (local-set-key "\t" 'markdown-cycle)
  (local-set-key [tab] 'yas/expand))

(add-hook 'markdown-mode-hook 'markdown-yas-fixup)
