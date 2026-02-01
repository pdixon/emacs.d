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

(require 'cl-lib)
(require 'json)
(require 'parse-time)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)

(defconst pinboard-host "api.pinboard.in")

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

(defvar pinboard-bookmarks-last-fetched nil
  "Last time all bookmarks were successfully fetched.")

(defvar pinboard-bookmarks nil
  "List of pinboard-bookmark structs.")

(defvar pinboard-tags nil
  "List of pinboard-tag-detail structs.")

;;; web api stuff

(defun pinboard-build-url (method &optional arguments)
  "Build url to call pinboard api METHOD with ARGUMENTS."
  (format "https://%s/v1/%s?%s"
          pinboard-host
          method
          (url-build-query-string
           `((format json)
             ,@arguments))))

(defun pinboard-request (method &optional arguments)
  "Synchronously call api METHOD with ARGUMENTS."
  (let ((url (pinboard-build-url method arguments)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (let ((status (thing-at-point 'line)))
        (unless (string-match "200" status)
          (error "Page not found: %s" status)))
      (when (boundp 'url-http-end-of-headers)
        (goto-char url-http-end-of-headers))
      (let ((json-object-type 'plist)
            (json-array-type 'list)
            (json-key-type 'keyword))
        (json-read)))))

(defun pinboard-request-posts-updated ()
  "Fetch time posts last updated."
  (when-let ((response (pinboard-request "posts/update")))
    (let ((time-string (plist-get response :update_time)))
      (parse-iso8601-time-string time-string))))

(defun pinboard-request-posts-all ()
  "Fetch all posts."
  (when-let ((response (pinboard-request "posts/all")))
    (mapcar
     (lambda (item)
       (let ((time (parse-iso8601-time-string (plist-get item :time)))
             (title (plist-get item :description))
             (url (plist-get item :href))
             (tags (split-string (plist-get item :tags))))
         (make-pinboard-bookmark
          :time time
          :title title
          :url url
          :tags tags)))
     response)))

(defun pinboard-request-tags-all ()
  "Fetch all tags."
  (when-let ((response (pinboard-request "tags/get")))
    ;; TODO figure out how to turn the plist in a list of tag-detail objects.
    ))

;;; bookmark list mode stuff

(defun pinboard-refresh-bookmarks ()
  "Update bookmark list if it's changed since last fetch."
  (when (or (not pinboard-bookmarks)
            (not pinboard-bookmarks-last-fetched)
            (time-less-p pinboard-bookmarks-last-fetched
                         (pinboard-request-posts-updated)))
    (when-let ((bookmarks (pinboard-request-posts-all)))
      (setq pinboard-bookmarks bookmarks)
      (setq pinboard-bookmarks-last-fetched (current-time)))))



(define-derived-mode pinboard-bookmark-list-mode
    tabulated-list-mode
    "Pinboard-Bookmarks"
  "Major mode for viewing lists of pinboard.in bookmarks.
\\{pinboard-bookmark-list-mode-map}"
  (setq tabulated-list-format
        (vector '("Time" 11 t)
                '("Tags" 30 t)
                '("Title" 46 t)))
  (setq tabulated-list-entries
       (lambda ()
         (mapcar (lambda (bookmark)
                   (list (pinboard-bookmark-url bookmark)
                         (vector
                          (format-time-string "%F"
                                              (pinboard-bookmark-time bookmark))
                          (mapconcat 'identity
                                     (pinboard-bookmark-tags bookmark) " ")
                          (pinboard-bookmark-title bookmark))))
                 pinboard-bookmarks)))
  (add-hook 'tabulated-list-revert-hook #'pinboard-refresh-bookmarks nil t)
  (tabulated-list-init-header)
  (let ((map pinboard-bookmark-list-mode-map))
    (define-key map (kbd "RET") #'pinboard-bookmark-open)))

(defun pinboard-bookmark-at-point ()
  "Get pinboard-book object for line at point."
  (if-let ((bookmark (seq-some-p
                      (lambda (item)
                        (string= (pinboard-bookmark-url item)
                                 (tabulated-list-get-id)))
                      pinboard-bookmarks)))
      bookmark
    (error "No bookmark at point")))

(defun pinboard-bookmark-open ()
  "Open the url of the current bookmark."
  (interactive)
  (if-let ((bookmark (pinboard-bookmark-at-point)))
      (browse-url (pinboard-bookmark-url bookmark))))

(defun pinboard-list-bookmarks ()
  "Show pinboard bookmarks in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*pinboard*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pinboard-bookmark-list-mode))
    (switch-to-buffer (current-buffer))
    (revert-buffer)))

;;; bookmark detail mode stuff

;;; tag list mode stuff

(defun pinboard-refresh-tags ()
  ""
  )

(define-derived-mode pinboard-tag-list-mode
    tabulated-list-mode
    "Pinboard-Tags"
  "Major mode for viewing lists of pinboard.in tags."
  (setq tabulated-list-format
        '(("Count" 10 t)
          ("Tag" 50) t))
  (setq tabulated-list-entries
        (lambda ()
          (mapcar (lambda (tag)
                    (list
                     (pinboard-tag-detail-name tag)
                     (vector
                      (format "%d"
                              (pinboard-tag-detail-count tag))
                      (pinboard-tag-detail-name tag))))
                  pinboard-tags)))
  (add-hook 'tabulated-list-revert-hook #'pinboard-refresh-tags nil t)
  (tabulated-list-init-header))

(provide 'pinboard)
;;; pinboard.el ends here
