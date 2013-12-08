;;; pd-darwin.el --- Mac specific setup

;; Copyright (C) 2012  Phillip Dixon

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

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'none)

(setq insert-directory-program "gls")

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; Fix up emacs macport info dir
(add-to-list 'Info-directory-list "/Applications/Emacs.app/Contents/Resources/info")

;; (set-face-attribute 'default nil :font "Anonymous Pro-12")
;; (pd/zenburn)

(server-start)
(setq magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")

(provide 'pd-darwin)
;;; pd-darwin.el ends here
