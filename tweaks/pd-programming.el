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
  :interpreter ("python" . python-mode)
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

(use-package eldoc
  :diminish eldoc-mode
  :defer t
  :config (eldoc-add-command 'paredit-backward-delete
                             'paredit-close-round))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :defer t)

(defun pd/elisp-mode-hook ()
  (paredit-mode)
  (elisp-slime-nav-mode)
  (eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'pd/elisp-mode-hook)

(provide 'pd-programming)
