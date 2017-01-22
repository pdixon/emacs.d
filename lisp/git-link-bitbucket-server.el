;;; git-link-bitbucket-server.el --- Bitbucket Server support for git-link  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: convenience, vc

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

(defun git-link-bitbucket-server (hostname dirname filename branch commit start end)
  ;; ?at=branch-name
  (format "http://%s/%s/src/%s/%s#%s-%s"
          hostname
          dirname
          commit
          filename
          (file-name-nondirectory filename)
          (if end
              (format "%s:%s" start end)
            start)))

(defun git-link-commit-bitbucket-server (hostname dirname commit)
  ;; ?at=branch-name
  (format "http://%s/%s/commits/%s"
          hostname
          dirname
          commit))

(provide 'git-link-bitbucket-server)
;;; git-link-bitbucket-server.el ends here
