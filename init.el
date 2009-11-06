(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)

(let* ((my-lisp-dir (concat dotfiles-dir "vendor"))
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(setq custom-file (concat dotfiles-dir "custom.el"))
(setq gnus-init-file (concat dotfiles-dir "dot-gnus.el"))

;; (add-to-list 'Info-directory-list (concat dotfiles-dir "vendor/share/info"))
;; (setenv "PYMACS_PYTHON" "~/.emacs.d/usr/bin/python")

;; Load up my config stuff
(setq tweaks-dir (concat dotfiles-dir "/tweaks"))
(add-to-list 'load-path tweaks-dir)
(mapc #'load (directory-files tweaks-dir nil ".*el$"))

;; load the customize stuff
(load custom-file 'noerror)

;; Work around a bug on OS X where system-name is FQDN
;;(if (eq system-type 'darwin)
;; This seems to be need on my linux box as well.
(setq my-system-name (car (split-string system-name "\\.")))

;; You can keep system-specific customizations here
(setq system-specific-config (concat dotfiles-dir my-system-name ".el"))
(if (file-exists-p system-specific-config) (load system-specific-config))
(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)
