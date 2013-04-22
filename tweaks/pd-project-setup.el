;;; pd-project-setup.el ---

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



(use-package find-file-in-project
  :bind ("C-c C-f" . ffip)
  :config
  (progn
    (require 'pd-project)
    (setq ffip-project-root-function 'pd-project-get-root)))


(provide 'pd-project-setup)
;;; pd-project-setup.el ends here
