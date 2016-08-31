;;; bitbucket-snippet.el --- create bitbucket snippets from emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Phillip Dixon

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

(require 'json)
(require 'url)
(require 'mm-url)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defconst bb--root "https://api.bitbucket.org/2.0/")

(defun bb-create-snippet (filename content)
  (let* ((boundary (mml-compute-boundary '()))
         (url-request-method "POST")
         (url-request-data (mm-url-encode-multipart-form-data (list (cons "file"
                                                                          (list (cons "name" "file")
                                                                                (cons "filename" filename)
                                                                                (cons "filedata" content)))
                                                                    (cons "title" filename)
                                                                    (cons "is_private" "true"))
                                                              boundary))
         (url-request-extra-headers (list (cons "Content-Type" (concat "multipart/form-data; boundary=" boundary))))
         (url (concat bb--root "snippets")))
    (with-current-buffer
        (url-retrieve-synchronously url)
      (goto-char (1+ url-http-end-of-headers))
      (bb--parse-json-response))))

(defun bb--parse-json-response ()
  (unless (eobp)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol)
          (json-false nil)
          (json-null nil))
      (json-read))))

(provide 'bitbucket-snippet)
;;; bitbucket-snippet.el ends here
