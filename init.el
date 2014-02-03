;;; init.el -- Emacs init file.

;; Copyright (C) 2011 Phillip Dixon

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

;; This is the entry point of the configuration. It it's up the load
;; paths and executes the rest of config files.

;;; Code:

(defconst *emacs-load-start* (current-time))
(message "Loading %s..." load-file-name)

(defconst dotfiles-dir (file-name-directory load-file-name))
(defconst tweaks-dir (concat dotfiles-dir "tweaks/"))
(defconst vendor-dir (concat dotfiles-dir "vendor/"))
(defconst lisp-dir (concat dotfiles-dir "lisp/"))
(defconst user-dir (concat dotfiles-dir "user/"))


;; You can keep system-specific customizations here
;; Use the only the inital term if the system name is a FQDN.
(defconst system-specific-config
      (concat user-dir (car (split-string system-name "\\.")) ".el"))

(add-to-list 'load-path tweaks-dir)
(add-to-list 'load-path lisp-dir)
(add-to-list 'load-path user-dir)

(setq custom-file (concat dotfiles-dir "custom.el"))
(setq gnus-init-file (concat dotfiles-dir "dot-gnus.el"))

;; load the customize stuff
(load custom-file 'noerror)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(require '10package)
(require 'use-package)

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Basic Config...done (%.3fs)" elapsed))

;; Basic Apperance
(if (not (eq system-type 'darwin))
    (menu-bar-mode 0))
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Load up my config stuff
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

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t
          auto-revert-verbose nil)))

(electric-pair-mode 1)
;;(electric-indent-mode 1)
;;(electric-layout-mode 1)

(savehist-mode t)

(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

(setq fill-column 78)

;; auto-complete in minibuffer
(icomplete-mode 1)

(set-default 'sentence-end-double-space nil)


;; Keep cursor away from edges when scrolling up/down
;;(require 'smooth-scrolling)
(mouse-wheel-mode t)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

(setq diff-switches "-u")


(use-package ido
  :init
  (ido-mode t)
  :config
  (progn
    (setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)))


(use-package 20misc)
(use-package 30defuns)

(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode 1)))

(use-package deft
  :bind ("<f5>" . deft)
  :config
  (progn
    (setq deft-extension "org")
    (setq deft-directory "~/personal/notes")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title t)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.mdwn\\'" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  (progn
    (setq markdown-command "pandoc")

    (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)))


(use-package pd-blog-helpers
  :commands (pd-blog-draft
             pd-blog-publish-post))

(use-package writegood-mode
  :commands (writegood-mode))

(use-package wc-mode
  :commands (wc-mode))

(use-package bibtex-mode
  :mode (("\\.bibtex\\'" . bibtex-mode)
         ("\\.bib\\'". bibtex-mode)))

(use-package ansi-term
  :bind ("<f10>" . pd-visit-term)
  :init
  (progn
    (defun pd-visit-term ()
      ""
      (interactive)
      (if (not (get-buffer "*ansi-term*"))
          (ansi-term (getenv "SHELL"))
        (switch-to-buffer "*ansi-term*"))))
  :config
  (progn
    (defun my-term-paste (&optional string)
      (interactive)
      (process-send-string
       (get-buffer-process (current-buffer))
       (if string
           string
         (current-kill 0))))

    (defun my-term-hook ()
      (goto-address-mode))
    (add-hook 'term-mode-hook 'my-term-hook)))

(use-package 50org-mode)
(use-package pd-autotyping)
(use-package pd-programming)

(use-package pd-darwin
  :if (eq system-type 'darwin))
(use-package pd-linux
  :if (eq system-type 'gnu/linux))

(if (file-exists-p system-specific-config)
    (load system-specific-config)
  (message "No system specific config for %s (i.e %s doesn't exist)"
           system-name
           system-specific-config))

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))
;;; init.el ends here
