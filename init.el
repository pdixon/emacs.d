(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/w3m"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/org/lisp"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/org/contrib/lisp"))
(add-to-list 'load-path (concat dotfiles-dir "/vendor/remember"))

(setq custom-file (concat dotfiles-dir "custom.el"))
(setq gnus-init-file (concat dotfiles-dir "dot-gnus.el"))

(setenv "PYMACS_PYTHON" "~/.emacs.d/usr/bin/python")

;; Load up my config stuff
(setq tweaks-dir (concat dotfiles-dir "/tweaks"))
(add-to-list 'load-path tweaks-dir)
(mapc #'load (directory-files tweaks-dir nil ".*el$"))

;; load the customize stuff
(load custom-file 'noerror)

;; Work around a bug on OS X where system-name is FQDN
(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

;; You can keep system-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el"))
(if (file-exists-p system-specific-config) (load system-specific-config))