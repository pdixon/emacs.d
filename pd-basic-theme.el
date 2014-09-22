(deftheme pd-basic
  "Created 2014-01-13.")

(let ((height (if (eq system-type 'darwin)
                  120
                90)))
  (custom-theme-set-faces
   'pd-basic
   `(default ((t (:height ,height :family "Source Code Pro"))))
   `(Info-quoted ((t (:weight bold))))))

(provide-theme 'pd-basic)
