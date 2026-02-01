;;; pd-magit-bitbucket.el --- Bitbucket helpers for magit  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: vc, tools

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

(require 'magit-git)

;; https://{stash-root}/{repo-slug}/pull-requests?create&targetBranch=refs%2Fheads%2Fmaster&sourceBranch=refs%2Fheads%2Ffeature%2FLA-375

;;;#autoload
(defun pd-magit-bb-visit-pull-request ()
  "Visit the current branch's PR on bitbucket."
  (interactive)
  (message
   (format "%s/pull-requests/new?source=%s"
           (replace-regexp-in-string
            "\\`\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-remote (magit-branch-at-point))
                       "url"))
           (cdr (or (magit-get-upstream-branch (magit-branch-at-point))
                    (user-error "No remote branch"))))))

(provide 'pd-magit-bitbucket)
;;; pd-magit-bitbucket.el ends here
