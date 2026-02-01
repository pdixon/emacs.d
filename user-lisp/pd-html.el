;;; pd-html.el --- Custom org-mode html exporter

;; Copyright (C) 2013  Phillip Dixon

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
(require 'ox-html)
(eval-when-compile (require 'cl))


(org-export-define-derived-backend 'pd-html 'html
  :translate-alist '((inner-template . pd-html-inner-template)
                     (template . pd-html-template)))

(defun pd-html-format-footnote-definition (fn)
  "Format the footnote definition FN."
  (let ((n (car fn)) (def (cdr fn)))
    (concat
     (format
      "<li id=\"fn.%s\">" n)
     def
     (format
      "<a href=\"#fnr.%s\">↩</a>" n)
     "</li>\n")))

(defun pd-html-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions
		    (plist-get info :parse-tree) info))
	 (fn-alist
	  (loop for (n type raw) in fn-alist collect
		(cons n (if (eq (org-element-type raw) 'org-data)
			    (org-trim (org-export-data raw info))
			  (format "<p>%s</p>"
				  (org-trim (org-export-data raw info))))))))
    (when fn-alist
      (concat
       "<section class=\"footnotes\">\n"
       "<hr />\n"
       "<ol>\n"
       (mapconcat 'pd-html-format-footnote-definition fn-alist "\n")
       "</ol>\n"
       "</section>\n"))))

(defun pd-html-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-html-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (pd-html-footnote-section info)))

(defun pd-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (format "<meta charset=\"%s\">\n"
           (coding-system-get org-html-coding-system 'mime-charset))
   (format "<title>%s</title>\n"
           (org-export-data (or (plist-get info :title) "") info))
   (format "<meta name=\"author\" content=\"%s\">\n"
           (org-export-data (plist-get info :author) info))
   (format "<meta name=\"generator\" content=\"Org-mode\">\n")
   "<link href=\"/css/style.css\" rel=\"stylesheet\" style=\"text/css\" />\n"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   "</head>\n"
   "<body>\n"
   ;; Preamble.
   "<header class=\"container\">\n"
   "<h1 href=\"/\">Phillip Dixon</h1>\n"
   "<nav>\n"
   "<ul>\n"
   "<li><a href=\"/\">Home</a></li>\n"
   "<li><a href=\"/posts/\">Posts</a></li>\n"
   "</ul>\n"
   "</nav>\n"
   "</header>\n"
   ;; Document contents.
   "<article class=\"container\">\n"
   ;; Document title.
   (format "<h1 class=\"title\">%s</h1>\n"
           (org-export-data (or (plist-get info :title) "") info))
   contents
   "</article>\n"
   ;; Postamble.
   "<footer>\n"
   "<div class=\"container\">\n"
   "<p>&#169; 2009&#8211;14 <a href=\"/\">Phillip Dixon</a> ( <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/3.0/nz/\">Some rights reserved</a> )</p>\n"
   (format "<p class=\"creator\">Generated with: %s</p>\n"
           (plist-get info :creator) info)
   "</div>\n"
   "</footer>\n"
   "</body>\n"
   "</html>\n"))

(defun pd-html-publish-to-html (plist filename pub-dir)
  "Publish an org file to pd custom HTML.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'pd-html filename ".html" plist pub-dir))

(provide 'pd-html)
;;; pd-blog-html.el ends here
