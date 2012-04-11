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
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(defvar pd-packages (list
                     'auctex
                     'cmake-mode
                     'color-theme-solarized
                     'deft
                     'expand-region
                     'gist
                     'graphviz-dot-mode
                     'haskell-mode
                     'iy-go-to-char
                     'lua-mode
                     'magit
                     'markdown-mode
                     'window-number
                     'yasnippet
))


(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p pd-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;(provide '(package-support))
;;; (10package.el) ends here
