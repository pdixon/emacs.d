;; Create my personal C style.

(require 'google-c-style)
(c-add-style "Google" google-c-style)

(defconst my-c-style
'("Google"
  (c-basic-offset . 4)))
(c-add-style "PERSONAL" my-c-style)

(defconst dcl-c-style
'("Google"
  (c-basic-offset . 3)))
(c-add-style "DCL" dcl-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (c-set-style "PERSONAL"))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
