;; Set up Flymake to use PyFlakes.
;; (eval-after-load "flymake"
;;   '(progn
;;      (defun flymake-pyflakes-init ()
;;        (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                           'flymake-create-temp-inplace))
;;               (local-file (file-relative-name
;;                            temp-file
;;                            (file-name-directory buffer-file-name))))
;;          (list "pyflakes" (list local-file))))
     
;;      (add-to-list 'flymake-allowed-file-name-masks
;;                   '("\\.py\\'" flymake-pyflakes-init))))

;; (add-hook 'python-mode-hook
;;           (lambda ()             
;;             (unless (eq buffer-file-name nil) (flymake-mode 1))))
