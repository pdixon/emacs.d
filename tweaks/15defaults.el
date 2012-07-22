;;; 15defaults.el --- The defaults I like

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

;; emacs has a lot of knobs to customise it's built in behaviour. This
;; turns them to the settings I like.

;;; Code:

(setq-default x-stretch-cursor t)

(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(show-paren-mode 1)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

(global-auto-revert-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)
(electric-layout-mode 1)

(savehist-mode t)

(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

(setq fill-column 78)

;; auto-complete in minibuffer
(icomplete-mode 1)

(put 'dired-find-alternate-file 'disabled nil)

(set-default 'sentence-end-double-space nil)


;; Keep cursor away from edges when scrolling up/down
;;(require 'smooth-scrolling)
(mouse-wheel-mode t)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

(setq diff-switches "-u")

(provide '15defaults)
;;; 15defaults.el ends here
