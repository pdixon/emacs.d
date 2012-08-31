;;; 30eproject.el --- Project stuff

;; Copyright (C) 2010 Phillip Dixon

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

(require 'eproject)
(require 'eproject-extras)

(define-project-type hg (generic)
  (look-for ".hg")
  :irelevant-files (".hg/"))

(define-project-type cmake (generic)
  (look-for "build")
  :relevant-files ("\\.cpp" "\\.h" "\\.txt")
  :irrelevant-files ("build/")
  :local-variables (lambda (root)
                     (list 'compile-command
                           (format "cd %s/build; make && make test" root))))

;; (defun eproject-cmake-ede-binding ()
;;   "Use the information we have from eproject to setup ede."
;;   (cond ((string= (eproject-type) "cmake")
;;          (message (format "Setting up ede cpp project in %s" (eproject-root)))
;;          (ede-cpp-root-project (eproject-name) :file (format "%sCMakeLists.txt" (eproject-root))))
;;         (t (message (format "No cmake project here, instead we have a %s" (eproject-type))))))

;; (add-hook 'eproject-first-buffer-hook 'eproject-cmake-ede-binding)

(define-project-type qt (generic)
  (look-for "*.pro" :glob)
  :relevant-files ("\\.cpp" "\\.h"))

(define-project-type python (generic)
  (look-for "setup.py")
  :relevant-files ("\\.py" "\\.h" "\\.c"))

(define-project-type haskell (generic)
  (look-for "*?.cabal" :glob)
  :relevant-files ("\\.hs" "\\.cabal" "\\.lhs" "\\.chs")
  :local-variables (lambda (root)
                     (list 'compile-command
                           (format "cd %s; cabal configure; cabal build" root))))

(define-project-type emacsd (generic)
  (look-for "init.el")
  :relevant-files ("\\.el")
  :irrelevant-files ("elpa/" "backups/"))

(define-project-type xcode (generic)
  (look-for "*.xcodeproj/project.pbxproj" :glob)
  :irrelevant-files ("DerivedData/")
  :local-variables (lambda (root)
                     (list 'compile-command
                           (format "cd %s; xcodebuild" root))))

(defun ibuffer-eproject-generate-filter-groups-by-name ()
  "Create a set of ibuffer filter groups based on the eproject root dirs of buffers"
  (mapcar (lambda (project-name)
            (cons (format "%s" project-name)
                  `((eproject . ,project-name))))
          (eproject-project-names)))

;;;###autoload
(defun ibuffer-eproject-set-filter-groups-by-name ()
  "Set the current filter groups to filter by eproject root dir."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-eproject-generate-filter-groups-by-name))
  (ibuffer-update nil t))

(defun all-projects-ibuffer (prefix)
  "Open an IBuffer window showing all buffers by project."
  (interactive "p")
  (ibuffer nil "*Projects*" nil nil nil (ibuffer-eproject-generate-filter-groups-by-name)))

(defun eproject-compile-dwim ()
  ""
  (interactive)
  (let ((comp-buffer-name (concat "*compilation: " (eproject-name) "*"))
        (default-directory (eproject-root)))
    (if (get-buffer comp-buffer-name)
        (with-current-buffer comp-buffer-name
          (recompile))
      (progn
        (compile compile-command)
        (with-current-buffer "*compilation*"
          (rename-buffer comp-buffer-name))))))

;;(define-key eproject-mode-map (kbd "C-c C-k") #'eproject-compile-dwim)
(assq-delete-all 'eproject-mode minor-mode-map-alist)

(use-package find-file-in-project
  :bind ("C-c C-f" . ffip)
  :config
  (progn
    (require 'eproject)
    (setq ffip-project-root-function 'eproject-root)))


;(require 'etags) ;; eproject-tags uses functions from this that don't autoload.
;(require 'eproject-tags)
;;; 30eproject.el ends here
