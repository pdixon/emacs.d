;;; 50autocomplete.el --- Setup for autocomplete

;; Copyright (C) 2010  Phillip Dixon

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

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-auto-start t)
(setq ac-dwim t)
(setq ac-override-local-map nil)

(setq-default ac-sources
              '(ac-source-yasnippet
                ac-source-abbrev))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (add-to-list 'ac-sources 'ac-source-symbols)
             (add-to-list 'ac-sources 'ac-source-functions)))

;; (ac-ropemacs-initialize)
;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (add-to-list 'ac-sources 'ac-source-ropemacs)))

;; (add-hook 'c-mode-common-hook
;;           '(lambda ()
;;              (add-to-list 'ac-sources 'ac-source-semantic)
;;              (add-to-list 'ac-sources 'ac-source-semantic-raw)))

(provide '50autocomplete)
;;; 50autocomplete.el ends here

