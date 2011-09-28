;;; (10package.el) --- 

;; Copyright (C) (2011)  Phillip Dixon

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

;; 

;;; Code:

(require 'cl)

(require 'package)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(defvar pd-packages (list 'magit
                          'gist
                          'auto-complete
                          'yasnippet-bundle
                          'autopair
                          'haskell-mode
                          'lua-mode
                          'graphviz-dot-mode
                          'deft
                          'markdown-mode))

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package pd-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; On your first run, this should pull in all the base packages.
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents))
  (starter-kit-elpa-install))

;;(provide '(package-support))
;;; (10package.el) ends here
