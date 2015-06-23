;;; stash.el --- Tools for working with Stash        -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: vc, convenience

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

(require 'magit)

(defun stash-remote-url ()
  (interactive)
  (message "%s" (url-generic-parse-url (magit-get "remote"
                                                  (magit-get-remote)
                                                  "url"))))

(defun stash-remote-branch ()
  (interactive)
  (message (cdr (magit-get-remote-branch))))

;; (defun endless/visit-pull-request-url ()
;;   "Visit the current branch's PR on Github."
;;   (interactive)
;;   (browse-url
;;    (format "https://github.com/%s/pull/new/%s"
;;      (replace-regexp-in-string
;;       "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
;;       (magit-get "remote"
;;                  (magit-get-remote)
;;                  "url"))
;;      (cdr (magit-get-remote-branch)))))

(provide 'stash)
;;; stash.el ends here
