;; Setup for yasnippets

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(setq tab-always-indent 'complete)

(use-package yasnippet
  :commands (yas-minor-mode yas-expand yas-hippie-try-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (hook-into-modes #'(lambda () (yas-minor-mode 1))
                   '(prog-mode-hook
                     org-mode-hook
                     message-mode-hook
                     markdown-mode-hook))
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt yas-complete-prompt))
    (setq yas-snippet-dirs (list (concat dotfiles-dir "snippets")))
    (yas-reload-all)))

(use-package hippie-exp
  :defer t
  :config
  (progn
    ;; Hippie expand: at times perhaps too hip
    (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
      (delete f hippie-expand-try-functions-list))

    ;; Add this back in at the end of the list.
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

(use-package company-mode
  :config
  (progn
    (setq company-begin-commands '(self-insert-command))
    (setq company-idle-delay 0.3)))

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

(provide 'pd-autotyping)
