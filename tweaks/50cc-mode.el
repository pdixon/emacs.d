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

(defun pd/toggle-header (&optional in-other-window)
  ""
  (interactive "P")
  (ff-get-other-file in-other-window))

(defun pd/toggle-test (&optional in-other-window)
  ""
  ;; This is based on ff-find-the-other-file, but without all the
  ;; customizable variables
  (interactive "P")
  (let (match     ;; matching regexp for this file.
        pos       ;; where the filenames start matching.
        suffixes  ;; replacement regexps for the matching regexp.
        format    ;; what we have to match.
        found     ;; name of the file or buffer found - nil if none.
        stub      ;; name of file without extension.
        (dirs '("../*")) ;; where to look for the file.
        (alist '(("Tests\\.m\\'" (".m"))
                 ("\\.m\\'" ("Tests.m")))) ;; candidate extensions.
        (fname (file-name-nondirectory buffer-file-name)) ;; basename
        )
    (setq match (car alist))
    ;; find the table entry corresponding to this file
    (setq pos (ff-string-match (car match) fname))
    (while (and match (if (and pos (>= pos 0)) nil (not pos)))
      (setq alist (cdr alist))
      (setq match (car alist))
      (setq pos (ff-string-match (car match) fname)))
    ;; otherwise, suffixes contains what we need
    (setq suffixes (car (cdr match))
          found nil)
    ;; otherwise build our filename stub
    (cond
     ;; get around the problem that 0 and nil both mean false!
     ((= pos 0)
      (setq format "")
      (setq stub "")
      )

     (t
      (setq format (concat "\\(.+\\)" (car match)))
      (string-match format fname)
      (setq stub (substring fname (match-beginning 1) (match-end 1)))
      ))

    (setq found
          (ff-get-file dirs
                       stub
                       suffixes
                       in-other-window))))
