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

(org-export-define-derived-backend 'pd-html 'html
  :translate-alist '((template . pd-html-template)))

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
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
   "<link href=\"/css/bootstrap.css\" rel=\"stylesheet\" style=\"text/css\" />\n"
   "</head>\n"
   "<body>\n"
   ;; Preamble.
   "<div class=\"navbar navbar-inverse navbar-static-top\">\n"
   "<div class=\"navbar-inner\">\n"
   "<div class=\"container\" >\n"
   "<a class=\"brand\" href\"/\">Phillip Dixon</a>\n"
   "<ul class=\"nav\">\n"
   "<li><a href=\"/\">Home</a></li>\n"
   "<li><a href=\"/posts/\">Posts</a></li>\n"
   "</ul>\n"
   "</div>\n"
   "</div>\n"
   "</div>\n"
   ;; Document contents.
   "<article class=\"container\">\n"
   ;; Document title.
   (format "<h1 class=\"title\">%s</h1>\n"
           (org-export-data (or (plist-get info :title) "") info))
   contents
   "</article>"
   ;; Postamble.
   "<footer class=\"container\">\n"
   "<p>&#169; 2009&#8211;12 <a href=\"/\">Phillip Dixon</a> ( <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-nc-nd/3.0/nz/\">Some rights reserved</a> )</p>\n"
   (format "<p>Generated with: %s</p>" (plist-get info :creator))
   "<footer>\n"
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
