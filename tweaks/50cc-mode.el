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
  (setq ff-always-in-other-window nil))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Compilation mode stuff
(defun pd/compilation-hook ()
  (setq truncate-lines t))

(add-hook 'compilation-mode-hook 'pd/compilation-hook)

(defun pd/objc-ff-setup-hook ()
  (set (make-local-variable 'cc-other-file-alist)
       '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp")))))

(add-hook 'objc-mode-hook 'pd/objc-ff-setup-hook)

(defun pd/toggle-header ()
  ""
  (interactive)
  (ff-find-other-file nil t))

(defun pd/xcode-target-dirs ()
  (if ((= (substring default-directory -6 -1) "Tests"))
      ((list "." (substring default-director 0 -6)))
    ((list "." (concat (substring default-directory 0 -1) "Tests")))))

(defun pd/toggle-test ()
  ""
  (interactive)
  (let
      ((ff-other-file-alist '((".m" ("Tests.m"))
                              ("Tests.m" (".m"))))
       (ff-always-try-to-create nil)
       (ff-search-directories (pd/xcode-target-dirs)))
    (ff-find-other-file nil t)))
