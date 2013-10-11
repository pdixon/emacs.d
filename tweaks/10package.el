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

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org-mode". "http://orgmode.org/elpa/"))
(package-initialize)

(require-package 'use-package)
(require-package 'applescript-mode)
(require-package 'auctex)
(require-package 'cmake-mode)
(require-package 'color-theme-solarized)
(require-package 'company)
(require-package 'deft)
(require-package 'diminish)
(require-package 'elisp-slime-nav)
(require-package 'expand-region)
(require-package 'fill-column-indicator)
(require-package 'find-file-in-project)
;;(require-package 'gist)
(require-package 'git-commit-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(require-package 'go-mode)
(require-package 'google-c-style)
(require-package 'graphviz-dot-mode)
(require-package 'haskell-mode)
(require-package 'htmlize)
(require-package 'ibuffer-vc)
;;(require-package 'iy-go-to-char)
(require-package 'lua-mode)
(require-package 'magit)
(require-package 'markdown-mode)
(require-package 'monky)
(require-package 'multiple-cursors)
(require-package 'org-plus-contrib)
(require-package 'pkgbuild-mode)
;;(require-package 'outline-magic)
;;(require-package 'python)
;;(require-package 'paredit)
;;(require-package 'smart-tab)
(require-package 'smartparens)
(require-package 'window-number)
(require-package 'yasnippet)
(require-package 'zenburn-theme)


(provide '10package)
;;; (10package.el) ends here
