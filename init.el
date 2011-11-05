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

(require 'cl)
(defvar *emacs-load-start* (current-time))

(defvar dotfiles-dir (file-name-directory load-file-name))
(defvar tweaks-dir (concat dotfiles-dir "tweaks/"))
(defvar vendor-dir (concat dotfiles-dir "vendor/"))

;; You can keep system-specific customizations here
;; Use the only the inital term if the system name is a FQDN.
(defvar system-specific-config
      (concat dotfiles-dir (car (split-string system-name "\\.")) ".el"))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path tweaks-dir)

;; Recusively add directories under vendor/ to the load path.
(let* ((my-lisp-dir vendor-dir)
       (default-directory my-lisp-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-lisp-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(add-to-list 'custom-theme-load-path (concat vendor-dir "solarized/"))

(setq custom-file (concat dotfiles-dir "custom.el"))
(setq gnus-init-file (concat dotfiles-dir "dot-gnus.el"))

;; load the customize stuff
(load custom-file 'noerror)

;; Load up my config stuff
(mapc 'load (directory-files tweaks-dir nil "^[^#].*el$"))

(when (file-exists-p system-specific-config)
  (load system-specific-config))

(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo)
              (+ (first *emacs-load-start*)
                 (second *emacs-load-start*)))))

;;; init.el ends here
