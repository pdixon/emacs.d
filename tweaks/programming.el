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
  :mode ("\\.ly\\'" . LilyPond-mode))
