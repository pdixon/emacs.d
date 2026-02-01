;;; xcdocset.el --- search in xcode docsets         -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: tools, docs, c

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

(defcustom xcdocset-document-path
  "~/Library/Developer/Shared/Documentation/DocSets/com.apple.adc.documentation.AppleiOS7.0.iOSLibrary.docset"
  "")

(defcustom xcdocset-exec
  "/Applications/Xcode.app/Contents/Developer/usr/bin/docsetutil"
  "")

(defun xcdocset-api-search (query)
  ""
  (interactive "sQuery Symbol: ")
  (let* ((result-string (with-output-to-string
                          (call-process xcdocset-exec nil standard-output nil
                                        "search" "-skip-text" "-query" (shell-quote-argument query) xcdocset-document-path)))
         (result-url (car (cdr (s-split "[ #]" result-string t))))
         (result-path (concat xcdocset-document-path "/Contents/Resources/Documents/" result-url)))
    (if result-url
        (eww-open-file result-path)
      (message "No API Docs for %s" query))))

(defun xcdocset-api-search-at-point ()
  ""
  (interactive)
  (xcdocset-api-search (thing-at-point 'symbol)))

(provide 'xcdocset)
;;; xcdoctset.el ends here
