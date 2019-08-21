(deftheme pd-fonts
  "My basic font settings.")

(let ((height (if (eq system-type 'darwin)
                  120
                100))
      (family (if (eq system-type 'darwin)
                "SF Mono"
              "Source Code Pro")))
  (custom-theme-set-faces
   'pd-fonts
   `(default ((t (:height ,height :family ,family))))
   `(fixed-pitch ((t (:height ,height :family ,family))))))

(provide-theme 'pd-fonts)
