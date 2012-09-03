;;; init.el -- Emacs init file.

;; Copyright (C) 2011 Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the entry point of the configuration. It it's up the load
;; paths and executes the rest of config files.

;;; Code:

(defconst *emacs-load-start* (current-time))
(message "Loading %s..." load-file-name)

(defconst dotfiles-dir (file-name-directory load-file-name))
(defconst tweaks-dir (concat dotfiles-dir "tweaks/"))
(defconst vendor-dir (concat dotfiles-dir "vendor/"))
(defconst lisp-dir (concat dotfiles-dir "lisp/"))

;; You can keep system-specific customizations here
;; Use the only the inital term if the system name is a FQDN.
(defconst system-specific-config
      (concat dotfiles-dir (car (split-string system-name "\\.")) ".el"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path tweaks-dir)
(add-to-list 'load-path lisp-dir)

;; Recusively add directories under vendor/ to the load path.
(let* ((my-lisp-dir vendor-dir)
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(setq custom-file (concat dotfiles-dir "custom.el"))
(setq gnus-init-file (concat dotfiles-dir "dot-gnus.el"))

;; load the customize stuff
(load custom-file 'noerror)

(require 'use-package)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Load up my config stuff
(mapc 'load (directory-files tweaks-dir nil "^[^#].*el$"))

(use-package pd-darwin
  :if (eq system-type 'darwin))
(use-package pd-linux
  :if (eq system-type 'gnu/linux))

(when (file-exists-p system-specific-config)
  (load system-specific-config))

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))
;;; init.el ends here
