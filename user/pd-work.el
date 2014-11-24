;;; pd-work.el --- Emacs settings for work machines.

;; Copyright (C) 2012  Phillip Dixon

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

(setq org-feed-alist
      '(("UniPD Trac Tickets" "http://tracunipd.au.ivc/report/7?format=rss&USER=pdixon"
         "~/org/inbox.org" "UniPD Assigned Issues")
        ("UniShark Trac Tickets" "http://tracunishark.au.ivc/report/7?format=rss&USER=pdixon"
         "~/org/inbox.org" "UniShark Assigned Issues")
        ("iChair Trac" "http://ichair.sw/report/7?format=rss&USER=pdixon"
         "~/org/inbox.org" "iChair Assigned Issues")))

(use-package mediawiki
  :config
  (progn
    (add-to-list 'mediawiki-site-alist
                 '("Software" "http://wiki.sw.au.ivc/mediawiki" "pdixon" "" "The PENSIEVE"))
    (setq mediawiki-site-default "Software")))

(provide 'pd-work)
;;; pd-work.el ends here
