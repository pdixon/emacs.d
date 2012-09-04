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

(use-package eproject
  :init
  (progn
    (use-package eproject-extras)
    (assq-delete-all 'eproject-mode minor-mode-map-alist)

    (use-package find-file-in-project
      :bind ("C-c C-f" . ffip)
      :config
      (progn
        (require 'eproject)
        (setq ffip-project-root-function 'eproject-root)))

    (use-package pd-eproject-extensions
      :bind (("C-c C-k" . eproject-compile-dwim)
             ("<f8>" . all-projects-ibuffer)))

    (define-project-type hg (generic)
      (look-for ".hg")
      :irelevant-files (".hg/"))

    (define-project-type cmake (generic-git hg)
      (look-for "build")
      :relevant-files ("\\.cpp" "\\.h" "\\.txt")
      :irrelevant-files ("build/")
      :local-variables (lambda (root)
                         (list 'compile-command
                               (format "cd %s/build; make && make test" root))))

    (define-project-type qt (generic-git hg)
      (look-for "*.pro" :glob)
      :relevant-files ("\\.cpp" "\\.h"))

    (define-project-type python (generic-git hg)
      (look-for "setup.py")
      :relevant-files ("\\.py" "\\.h" "\\.c"))

    (define-project-type haskell (generic-git hg)
      (look-for "*?.cabal" :glob)
      :relevant-files ("\\.hs" "\\.cabal" "\\.lhs" "\\.chs")
      :local-variables (lambda (root)
                         (list 'compile-command
                               (format "cd %s; cabal configure; cabal build" root))))

    (define-project-type emacsd (generic-git hg)
      (look-for "init.el")
      :relevant-files ("\\.el")
      :irrelevant-files ("elpa/" "backups/"))

    (define-project-type xcode (generic-git hg)
      (look-for "*.xcodeproj/project.pbxproj" :glob)
      :irrelevant-files ("DerivedData/")
      :local-variables (lambda (root)
                         (list 'compile-command
                               (format "cd %s; xcodebuild" root))))))

;;; 30eproject.el ends here
