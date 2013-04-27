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

(defgroup pd-project nil
  "PD Project settings."
  :group 'programming)

(defcustom pd-project-root-changed-hook nil
  "Hook to run when the current project root is set or changed."
  :type 'hook
  :group 'pd-project)

(defcustom pd-project-vc-root-marker '(".git" ".hg")
  "Files or directories present in the root of a repository."
  :type '(repeat string)
  :group 'pd-project)

(defcustom pd-project-todo-regexp
  '("TODO" "HACK" "FIXME")
  "A list of terms for `pd-project-todo' to search for."
  :group 'pd-proj
  :type '(repeat string))

;;;###autoload
(defun pd-project-compile ()
  "Call compile at the project root."
  (interactive)
  (let ((comp-buffer-name (concat "*compilation: " (pd-project-get-root) "*"))
        (default-directory (pd-project-get-root)))
    (if (get-buffer comp-buffer-name)
        (with-current-buffer comp-buffer-name
          (recompile))
      (progn
        (compile compile-command)
        (with-current-buffer "*compilation*"
          (rename-buffer comp-buffer-name))))))

;;;###autoload
(defun pd-project-grep (regexp)
  "Search all project files for REGEXP."
  (interactive "sRegexp grep: ")
  (let ((root (pd-project-get-root)))
    (rgrep regexp "* .*" root)))

;;;###autoload
(defun pd-project-todo ()
  "Find TODO type comments in the project.

`pd-project-todo-regexp' is used to determine what this function looks for."
  (interactive)
  (pd-project-grep (regexp-opt pd-project-todo-regexp)))

;;;###autoload
(defun pd-project-find-file ()
  "Open a file in the project."
  (interactive))

(defvar pd-project-root nil
  "The current project root.")
(make-variable-buffer-local 'pd-project-root)
(put 'pd-project-root 'safe-local-variable 'file-directory-p)

;;;###autoload
(defun pd-project-get-root ()
  "Return the current project root directory.

If there is no current project root find one using pd-project-guess-root."
  (when (not pd-project-root)
    (pd-project-set-root  (pd-project-guess-root)))
  pd-project-root)

;;;###autoload
(defun pd-project-set-root (new-root)
  "Set the current project root directory to NEW-ROOT."
  (interactive "DProject root: ")
  (setq pd-project-root new-root)
  (run-hooks 'pd-project-root-changed-hook))

(defun pd-project-guess-root ()
  "Guess the project root from version control files."
  (locate-dominating-file
   (buffer-file-name)
   (lambda (dir)
     (catch 'return
       (dolist (marker pd-project-vc-root-marker)
         (when (file-exists-p (concat dir "/" marker))
           (throw 'return t)))
       nil))))

(provide 'pd-project)
;;; pd-project.el ends here
