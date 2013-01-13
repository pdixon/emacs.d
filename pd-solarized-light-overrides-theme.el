(deftheme pd-solarized-light-overrides
  "Created 2012-09-04.")

(custom-theme-set-faces
 'pd-solarized-light-overrides
 '(org-todo ((t (:background "#fcf4dc"
                             :foreground "#c60007"
                             :inverse-video nil
                             :underline nil
                             :slant normal
                             :weight bold))))
 '(term-color-black ((t (:foreground "#073642"
                         :background "#073642"))))
 '(term-color-red ((t (:foreground "#dc323f"
                       :background "#dc323f"))))
 '(term-color-green ((t (:foreground "#859900"
                         :background "#859900"))))
 '(term-color-yellow ((t (:foreground "#b58900"
                          :background "#b58900"))))
 '(term-color-blue ((t (:foreground "#268bd2"
                        :background "#268bd2"))))
 '(term-color-magenta ((t (:foreground "#d33682"
                           :background "#d33682"))))
 '(term-color-cyan ((t (:foreground "#2aa198"
                        :background "#2aa198"))))
 '(term-color-white ((t (:foreground "#eee8d5"
                         :background "#eee8d5")))))

(provide-theme 'pd-solarized-light-overrides)
