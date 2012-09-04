;; Setup for yasnippets

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(use-package yasnippet
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (hook-into-modes #'(lambda () (yas-minor-mode 1))
                   '(prog-mode-hook
                     org-mode-hook
                     message-mode-hook
                     markdown-mode-hook))
  :config
  (progn
    (setq yas-root-directory (concat dotfiles-dir "snippets"))
    (yas-reload-all)))

(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory (concat dotfiles-dir "mytemplates/"))
(setq auto-insert-query nil)

(defun pd-expand-buffer ()
  "Expand buffer in place as a yasnippet."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(define-auto-insert "\\.markdown\\'"
  ["post.markdown" pd-expand-buffer])

(define-auto-insert "\\.mdwn\\'"
  ["template.mdwn" pd-expand-buffer])

(define-auto-insert "\\.m\\'"
  ["template.m" pd-expand-buffer])
