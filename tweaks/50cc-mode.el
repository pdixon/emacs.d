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
  (c-set-style "PERSONAL")
  (setq ff-always-in-other-window t))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Compilation mode stuff
(defun pd/compilation-hook ()
  (setq truncate-lines t))

(add-hook 'compilation-mode-hook 'pd/compilation-hook)

(defun pd/objc-ff-setup-hook ()
  (set (make-local-variable 'cc-other-file-alist)
       '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp")))))

(add-hook 'objc-mode-hook 'pd/objc-ff-setup-hook)

;; (define-project-type kernel (generic-git) (look-for "Kbuild"))
