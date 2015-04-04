;;; pinboard.el --- pinboard.in client for emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; 

;; This file is NOT part of GNU Emacs

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

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'tabulated-list)
(require 'url)

(defconst pinboard-host "pinboard.in")

(cl-defstruct pinboard-bookmark
  title
  tags
  url
  unread
  time
  annotation
  shared)

(cl-defstruct pinboard-tag-detail
  name
  count)

(defvar pinboard-bookmarks nil
  "List of pinboard-bookmark structs.")

(defvar pinboard-tags nil
  "List of pinboard-tag-detail structs.")

;;; web api stuff

(defun pinboard-credentials ()
  "Look up credentials in auth-source."
  (let* ((auth-source-creation-prompts
          '((user . "pinboard.in username: ")
            (secret . "pinboard.in password: ")))
         (found (nth 0 (auth-source-search :max 1
                                           :host pinboard-host
                                           :require '(:user :secret)
                                           :create t))))
    (if found
        (list (plist-get found :user)
              (let ((secret (plist-get found :secret)))
                (if (functionp secret)
                    (funcall secret)
                  secret))
              (plist-get found :save-function))
      nil)))

(defun pinboard-build-url (method user password &optional arguments)
  ""
  (format "https://%s:%s@api.%s/v1/%s?%s"
          user
          password
          pinboard-host
          method
          (url-build-query-string
           `((format json)
             ,@arguments))))

(defun pinboard-request (method callback &optional arguments)
  ""
  (let* ((creds (pinboard-credentials))
         (creds-save-func (nth 2 creds))
         (url (pinboard-build-url method (nth 0 creds) (nth 1 creds) arguments)))
    (url-retrieve
     url
     (lambda (_)
       (goto-char (point-min))
       (unless (string-match "200" (thing-at-point 'line))
         (error "Page not found: %s" (thing-at-point 'line)))
       (when (boundp 'url-http-end-of-headers)
         (goto-char url-http-end-of-headers))
       (funcall callback (let ((json-object-type 'plist)
                               (json-array-type 'list)
                               (json-key-type 'keyword))
                           (json-read)))
       (when creds-save-func
         (funcall creds-save-func)))
     nil t)))

;;; bookmark list mode stuff

(defun pinboard-refresh-bookmarks ()
  ""
  )

(defun pinboard-bookmark-list-entry (bookmark)
  ""
  (list
   (pinboard-bookmark-url bookmark)
   (vector
    (pinboard-bookmark-time bookmark)
    (pinboard-bookmark-tags bookmark)
    (pinboard-bookmark-title bookmark))))

(defun pinboard-bookmark-list-entries ()
  ""
  (seq-map #'pinboard-bookmark-list-entry pinboard-bookmarks))

(define-derived-mode pinboard-bookmark-list-mode
    tabulated-list-mode
    "Pinboard-Bookmarks"
  "Major mode for viewing lists of pinboard.in bookmarks."
  (setq tabulated-list-format
        '(("Time" 13 t)
          ("Tags" 30 t)
          ("Title" 46 t)))
  (setq tabulated-list-entries #'pinboard-bookmark-list-entries)
  (add-hook 'tabulated-list-revert-hook #'pinboard-refresh-bookmarks nil t)
  (tabulated-list-init-header))

;;; bookmark detail mode stuff


;;; tag list mode stuff

(defun pinboard-refresh-tags ()
  ""
  )

(defun pinboard-tag-list-entries ()
  ""
  )

(define-derived-mode pinboard-tag-list-mode
    tabulated-list-mode
    "Pinboard-Tags"
  "Major mode for viewing lists of pinboard.in tags."
  (setq tabulated-list-format
        '(("Count" 10 t)
          ("Tag" 50) t))
  (setq tabulated-list-entries #'pinboard-tag-list-entries)
  (add-hook 'tabulated-list-revert-hook #'pinboard-refresh-tags nil t)
  (tabulated-list-init-header))

(provide 'pinboard)
;;; pinboard.el ends here
