
(defun pd-python-mode-hook ()
  (electric-indent-mode -1) ;; This isn't useful in python
  )

(add-hook 'python-mode-hook 'pd-python-mode-hook)

(provide 'pd-python)

;;; 50python.el ends here
