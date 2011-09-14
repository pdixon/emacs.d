;;; 50blog-helpers.el --- Helper functions for working with my blog

;; Copyright (C) 2011  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: wp, convenience

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

;; based on https://github.com/metajack/jekyll/blob/master/emacs/jekyll.el

;;; Code:

(defvar blog-dir "~/Documents/phil.dixon.gen.nz/"
  "Path to blog.")
(defvar blog-drafts-dir "drafts/"
  "Relative path to drafts directory.")
(defvar blog-posts-dir "posts/"
  "Relative path to posts directory.")
(defvar blog-post-ext ".markdown"
  "File extension of blog posts.")

(defun pd-make-slug (s)
  "Make a string in to a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun pd-blog-draft (title)
  "Create a new blog post."
  (interactive "sPost Title: ")
  (message title)
  (let ((draft-file (concat blog-dir blog-drafts-dir
                            (pd-make-slug title)
                            blog-post-ext)))
    (find-file draft-file)))

(defun pd-blog-is-draft-p (filename)
  "Is the current buffer a draft post"
  (equal (file-name-directory filename)
         (expand-file-name (concat blog-dir blog-drafts-dir))))

(defun pd-blog-publish-post ()
  "Move a post from the `blog-drafts-dir` to `blog-posts-dir` and prepend the date
to the name."
  (interactive)
  (cond
   ((not (pd-blog-is-draft-p (buffer-file-name (current-buffer))))
    (message "This is not a draft post."))
   ((buffer-modified-p)
    (message "Buffer has modifications, can't publish post"))
   (t
    (let ((filename
           (concat blog-dir blog-posts-dir
                   (format-time-string "%Y-%m-%d-")
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer))))))
      (rename-file-and-buffer filename)))))

(provide '50blog-helpers)
;;; 50blog-helpers.el ends here
