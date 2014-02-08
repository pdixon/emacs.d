(defvar my-yas-after-expand-sexp nil)

(defmacro my-yas-after-expand (&rest body)
 `(progn
    (setq my-yas-after-expand-sexp
          '(progn
             (save-restriction
               (narrow-to-region yas-snippet-beg yas-snippet-end)
               (goto-char yas-snippet-beg)
               ,@body
               (setq yas-snippet-end (point-max))
               (goto-char (point-max)))))
    t))

(add-hook 'yas-after-exit-snippet-hook
         (defun my-yas-after-exit-handler ()
                (eval my-yas-after-expand-sexp)
                (setq my-yas-after-expand-sexp nil)))
