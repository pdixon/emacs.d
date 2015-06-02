;;; pd-project.el --- Project Operations

;; Copyright (C) 2013  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: tools, vc, matching, convenience

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

;; The Emacs world is a wash with various ways for dealing with
;; projects. This is my contribution. It's really unique only in that
;; it doesn't do much. It equates a version controlled directory tree
;; with a project.
;;
;; https://raw.github.com/jorgenschaefer/project-el/master/project.el

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 's)

(defgroup pd-project nil
  "PD Project settings."
  :group 'programming)

(defcustom pd-project-todo-regexp
  '("TODO" "HACK" "FIXME")
  "A list of terms for `pd-project-todo' to search for."
  :group 'pd-proj
  :type '(repeat string))

;;;###autoload
(defun pd-project-compile ()
  "Call compile at the project root."
  (interactive)
  (when-let (root (pd-project-get-root))
    (let ((comp-buffer-name (concat "*compilation: " root "*"))
          (default-directory root))
      (if (get-buffer comp-buffer-name)
          (with-current-buffer comp-buffer-name
            (recompile))
        (progn
          (compile compile-command)
          (with-current-buffer "*compilation*"
            (rename-buffer comp-buffer-name)))))))

;;;###autoload
(defun pd-project-grep (regexp)
  "Search all project files for REGEXP."
  (interactive "sRegexp grep: ")
  (when-let (root (pd-project-get-root))
    (vc-git-grep regexp "* .*" root)))

;;;###autoload
(defun pd-project-todo ()
  "Find TODO type comments in the project.

`pd-project-todo-regexp' is used to determine what this function looks for."
  (interactive)
  (pd-project-grep (regexp-opt pd-project-todo-regexp)))


(defun pd-completing-read (prompt collection)
  (let (rlt)
    (cond
     ( (= 1 (length collection))
       ;; open file directly
       (setq rlt (car collection)))
     ((and (boundp 'ido-mode) ido-mode)
      (setq rlt (ido-completing-read prompt collection)))
     (t
      (setq rlt (completing-read prompt collection))))
    rlt))

(defun pd-project-files ()
  "Return an alist of all filenames in the project and their path.

Files with duplicate filenames are suffixed with the name of the
directory they are found in so that they are unique."
  (let (file-alist
        (old-dir default-directory))
    (with-temp-buffer
      (cd (pd-project-get-root))
      (vc-git-command t nil "ls-files" nil)
      (goto-char (point-min))
      (cl-loop until (eobp)
               do ((lambda ()
                     (let* ((file (s-trim (thing-at-point 'line)))
                            (file-cons (cons (file-name-nondirectory file)
                                             (expand-file-name file))))
                        (add-to-list 'file-alist file-cons))))
               (forward-line 1))
      (cd old-dir))
    file-alist))

(defun pd-find-files ()
  (let* ((project-files (pd-project-files))
         (files (mapcar 'car project-files))
         file
         root)
    (cond
     ((and files (> (length files) 0))
      (setq root (file-name-nondirectory (directory-file-name (pd-project-get-root))))
      (setq file (pd-completing-read (format "Find file in %s/: " root)  files))
      (find-file (cdr (assoc file project-files))))
     (t (message "No match file exist!")))))


;;;###autoload
(defun pd-project-find-file ()
  "Open a file in the project."
  (interactive)
  (pd-find-files))

(defun pd-project-get-root ()
    ""
    (vc-find-root (or (buffer-file-name) default-directory) ".git"))

(provide 'pd-project)
;;; pd-project.el ends here
