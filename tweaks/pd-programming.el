(use-package haskell-mode
  :mode ("\\.l?hs\\'" . haskell-mode)
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter (("lua" . lua-mode)
                ("luajit" . lua-mode)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter (("python" . python-mode)
                ("python3" . python-mode))
  :config
  (progn
    (defun pd-python-mode-hook ()
      (electric-indent-mode -1) ;; This isn't useful in python
      )

    (add-hook 'python-mode-hook 'pd-python-mode-hook)))

(use-package wizard-db
  :mode ("\\.xmd\\'" . wizard-db-mode))

(use-package lilypond-mode
  :load-path "vendor/lilypond"
  :mode ("\\.ly\\'" . LilyPond-mode))

(use-package pkgbuild-mode
  :mode ("PKGBUILD" . pkgbuild-mode))

(use-package conf-mode
  :mode ("hgrc" . conf-mode))

(use-package cc-mode
  :defer t
  :config
  (progn
    (use-package google-c-style
      :load-path "vendor/"
      :init
      (progn
        (c-add-style "Google" google-c-style)

        (defconst my-c-style
          '("Google"
            (c-basic-offset . 4)
            (c-offsets-alist . ((inextern-lang . -)))))
        (c-add-style "PERSONAL" my-c-style)

        (defconst dcl-c-style
          '("Google"
            (c-basic-offset . 3)))
        (c-add-style "DCL" dcl-c-style)))

    (defconst my-obj-c-style
      '("bsd"
        (c-basic-offset . 4)
        (indent-tabs-mode . nil)
        (c-offsets-alist . ((case-label . +)))))
    (c-add-style "my-obj-c" my-obj-c-style)

    ;; Customizations for all modes in CC Mode.
    (defun my-c-mode-common-hook ()
      (c-set-style "PERSONAL")
      (setq ff-always-in-other-window nil))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

    (defun pd/objc-ff-setup-hook ()
      (set (make-local-variable 'cc-other-file-alist)
           '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp")))))

    (add-hook 'objc-mode-hook 'pd/objc-ff-setup-hook)

    (use-package pd-cc-mode-extras
      :commands (pd/toggle-header
                 pd/toggle-test))))

(use-package compile
  :defer t
  :config
  (progn
    (defun pd/compilation-hook ()
      (setq truncate-lines t))

    (add-hook 'compilation-mode-hook 'pd/compilation-hook)))

(use-package lisp-mode
  :config
  (progn
    (use-package eldoc
      :diminish eldoc-mode
      :defer t)

    (use-package elisp-slime-nav
      :diminish elisp-slime-nav-mode
      :defer t)

    (defun pd/elisp-mode-hook ()
      (elisp-slime-nav-mode)
      (eldoc-mode))

    (add-hook 'emacs-lisp-mode-hook 'pd/elisp-mode-hook)))

;; From emacs start kit v2.
;;; These belong in prog-mode-hook:

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.

(defun pd/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun pd/turn-on-hl-line-mode ()
  (when window-system (hl-line-mode t)))

(defun pd/turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun pd/turn-on-whitespace ()
  (whitespace-mode t))

(defun pd/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun pd/turn-on-which-func ()
  (which-function-mode t))

(add-hook 'prog-mode-hook 'pd/local-comment-auto-fill)
(add-hook 'prog-mode-hook 'pd/turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'pd/turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'pd/turn-on-whitespace)
(add-hook 'prog-mode-hook 'pd/add-watchwords)
(add-hook 'prog-mode-hook 'pd/turn-on-which-func)

(provide 'pd-programming)
