;;; init.el -- Emacs init file.

;; Copyright (C) 2011-2014 Phillip Dixon

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

(setq message-log-max 10000)

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'subr-x)

(defconst *emacs-load-start* (current-time))
(message "Loading %s..." load-file-name)

(defconst dotfiles-dir (file-name-directory load-file-name))
(defconst lisp-dir (concat dotfiles-dir "lisp/"))
(defconst user-dir (concat dotfiles-dir "user/"))

;; You can keep system-specific customizations here
;; Use the only the inital term if the system name is a FQDN.
(defconst system-specific-config
  (concat user-dir (car (split-string (system-name) "\\.")) ".el"))

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

(when (equal (car (split-string (system-name) "\\.")) "bigMacDev")
  (setq url-proxy-services '(("no_proxy" . "\\.au.ivc")
                             ("http" . "127.0.0.1:3128")
                             ("https" . "127.0.0.1:3128"))))

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Basic Config...done (%.3fs)" elapsed))

;; package.el setup
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  ;(setq use-package-verbose t)
  )
(require 'bind-key)
(require 'diminish)

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Package Config...done (%.3fs)" elapsed))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package dynamic-fonts
  :ensure t
  :defines (dynamic-fonts-preferred-monospace-fonts
            dynamic-fonts-preferred-monospace-point-size
            dynamic-fonts-preferred-proportional-fonts
            dynamic-fonts-preferred-proportional-point-size)
  :init
  (setq dynamic-fonts-preferred-monospace-fonts
        '("Source Code Pro"
          "Inconsolata"
          "Consolas"
          "Menlo"
          "DejaVu Sans Mono"
          "Bitstream Vera Mono"))
  (setq dynamic-fonts-preferred-monospace-point-size
        (pcase system-type
          (`darwin 12)
          (_ 9)))
  (setq dynamic-fonts-preferred-proportional-fonts
        '("Helvetica"
          "Segoe UI"
          "DejaVu Sans"
          "Bitstream Vera"))
  (setq dynamic-fonts-preferred-proportional-point-size
        (pcase system-type
          (`darwin 12)
          (_ 9)))
  (dynamic-fonts-setup))

(use-package unicode-fonts
  :disabled t
  :ensure t
  :init (unicode-fonts-setup))

(use-package zenburn
  :disabled t
  :ensure zenburn-theme
  :defer t
  :init
  (load-theme 'zenburn t))

(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (setq solarized-distinct-fringe-background t
        solarized-high-contrast-mode-line t
        solarized-use-less-bold t
        solarized-use-more-italic t
        solarized-use-variable-pitch nil
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)
    (load-theme 'solarized-light t))

;; Basic Apperance
;; (if (not (eq system-type 'darwin))
;;     (menu-bar-mode 0))
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq use-dialog-box nil)

;; Load up my config stuff
(setq inhibit-startup-message t)

(fset 'yes-or-no-p #'y-or-n-p)
(fset 'display-startup-echo-area-message #'ignore)

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

(when window-system
  (setq frame-resize-pixelwise t
        frame-title-format '(buffer-file-name "emacs - %f" ("emacs - %b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(use-package frame
  :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;; Apperance

(setq vc-handled-backends '(Git Hg))
(setq whitespace-style '(face trailing tabs)
      whitespace-line-column 80)


(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "phil@dixon.gen.nz")
(setq user-full-name "Phillip Dixon")

(electric-pair-mode 1)
;;(electric-indent-mode 1)
;;(electric-layout-mode 1)

(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)

(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

(setq fill-column 78)

(set-default 'sentence-end-double-space nil)

;; Keep cursor away from edges when scrolling up/down
;;(require 'smooth-scrolling)
(mouse-wheel-mode t)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(setq indicate-empty-lines t
      require-final-newline t)

(setq view-read-only t)

(setq diff-switches "-u")

;; make scripts executable on save.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode 1)))

(setq tab-always-indent 'complete)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'none)

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Non-use package stuff...done (%.3fs)" elapsed))

;; Save a list of recent files visited.
(use-package recentf
  :defer 1
  :config
  (setq  recentf-auto-cleanup 300
         recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                               "/elpa/.*\\'" ; Package files
                               )))

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t
          auto-revert-check-vc-info t
          auto-revert-verbose nil)))

(use-package savehist
  :init (savehist-mode t))

(bind-keys :prefix-map my-toggle-map
           :prefix "C-x t"
           ("c" . pd-cleanroom-mode)
           ("f" . auto-fill-mode)
           ("r" . dired-toggle-read-only)
           ("w" . whitespace-mode)
           ("v" . visual-line-mode))

(use-package applescript-mode
  :defer t
  :ensure t)

(use-package auctex
  :defer t
  :ensure t)

(use-package cmake-mode
  :defer t
  :ensure t)

(use-package go-mode
  :defer t
  :ensure t)

(use-package graphviz-dot-mode
  :defer t
  :ensure t)

(use-package ibuffer-vc
  :defer t
  :ensure t)

(use-package window-number
  :defer t
  :ensure t)

(use-package delsel
  :defer t
  :init (delete-selection-mode))

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
          ido-max-prospects 10)))

(use-package flx-ido
  :ensure t
  :init
  (flx-ido-mode))

(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode))

(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode))

(use-package smex
  :ensure t
  :bind (([remap execute-extended-command] . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package eudc
  :defer t
  :config
  (progn
    (eudc-set-server "localhost" 'mab t)
    (eudc-protocol-set 'eudc-inline-expansion-format
                       '("%s %s <%s>" firstname lastname email)
                       'mab)))

(use-package message
  :defer t
  :config
  (require 'eudc)
  (setq message-send-mail-function 'smtpmail-send-it
        message-kill-buffer-on-exit t))

(use-package sendmail
  :defer t
  :config
  (setq send-mail-function 'smtpmail-send-it ))

(use-package smtpmail
  :defer t
  :config
  (setq smtpmail-stream-type 'ssl
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465))

(use-package ispell
  :defer t
  :config
  (setq ispell-dictionary "en_GB-ise"
        ispell-extra-args `("--keyboard=dvorak")
        ispell-silently-savep t))

(use-package imenu
  :defer t
  :config
  (progn
    (setq imenu-max-items 200)))

(use-package ibuffer
  :defer t
  :bind ("<f8>" . ibuffer)
  :init
  (defalias 'list-buffers 'ibuffer)
  :config
  (setq ibuffer-expert 1)
  (setq ibuffer-show-empty-filter-groups nil)

  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

    ;; Use human readable Size column instead of original one

  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

    ;; Modify the default ibuffer-formats

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))


(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))


(use-package files
  :defer t
  :config
  (when-let (gls (and (eq system-type 'darwin) (executable-find "gls")))
    (setq insert-directory-program gls)))

(use-package dired
  :defer t
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-dwim-target t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-listing-switches "-alhF")
  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    ;; If we are on a GNU system or have GNU ls, add some more `ls' switches:
    ;; `--group-directories-first' lists directories before files, and `-v'
    ;; sorts numbers in file names naturally, i.e. "image1" goes before
    ;; "image02"
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v")))

  (defun pd/dired-do-multi-occur (regexp)
    "Show all in lines in marked files containing REGEXP"
    (interactive "MList lines matching regexp: ")
    (multi-occur (mapcar 'find-file (dired-get-marked-files)) regexp))

  (defun pd-dired-find-alternate-parent ()
    (interactive)
    (find-alternate-file ".."))

  (bind-key "^" 'pd-dired-find-alternate-parent dired-mode-map))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(use-package expand-region
  :ensure t
  :bind (("M-\"" . er/contract-region)
         ("M-'" . er/expand-region)))

(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package hungry-delete
  :ensure t
  :init
  (global-hungry-delete-mode)
  :config
  (setq hungry-delete-chars-to-skip " \t"))

(use-package pd-centered-window
  :load-path "lisp/"
  :commands (pd-centered-window))

(use-package pd-cleanroom
  :load-path "lisp/"
  :commands (pd-cleanroom-mode))

(use-package magit
  ;:ensure t
  :load-path "site-lisp/magit"
  :bind ("<f7>" . magit-status)
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package hg-commit-mode
  :mode ("hg-editor-.*\\.txt\\'" . hg-commit-mode))

(use-package ediff
  :defer t
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package pd-editing-extras
  :bind (("C-c +" . my-increment)
         ("C-t" . transpose-dwim)
         ("M-c". toggle-letter-case)
         ("M-<SPC>" . cycle-spacing)))

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

(bind-key "M-J" 'delete-indentation-forward)
(bind-key "M-j" 'delete-indentation)

(bind-key "C-h a" 'apropos)

(bind-key "C-c y" 'bury-buffer)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(use-package bookmark
  :bind ("<f9>" . bookmark-bmenu-list))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)))

(use-package paredit
  :ensure t
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;; Yank line or region
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Kill line or region
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(use-package hexl-mode
  :mode (("\\.exe\\'" . hexl-mode)
         ("\\.dll\\'" . hexl-mode)))

(use-package find-file-in-project
  :ensure t
  :bind ("C-c C-f" . ffip)
  :config
  (require 'pd-project)
  (setq ffip-project-root-function 'pd-project-get-root))

(use-package pd-project
  :bind(("C-c b" . pd-project-compile))
  :commands (pd-project-grep
             pd-project-todo))

(use-package pd-window-extras
  :commands (pd/rotate-windows
             pd/toggle-window-split
             pd/setup-windows
             pd/toggle-just-one-window))

(defun my-kill-word ()
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-syntax-backward "w_")
      (setq p1 (point))
      (skip-syntax-forward "w_")
      (setq p2 (point))
      (kill-region p1 p2))))

(defun my-copy-line (arg)
  "Copy ARG lines in to the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun my-isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

(defun my-search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun my-search-word-forward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))


(defun esk-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;; From https://github.com/bbatsov/emacs-prelude
(defun prelude-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

;; AppleScript Safari stuff
(defun tell-app (app something)
  "Use Applescript to tell APP to do SOMETHING."
  (decode-coding-string
   (do-applescript
    (concat "tell application\""
            app
            "\" to "
            something))
   'mac-roman))

(defun my-safari-selection ()
  (tell-app "Safari"
            "do Javascript \"getSelection()\" in front document"))

(defun my-safari-url ()
  (tell-app "Safari"
            "URL of front document"))

(defun my-safari-title ()
  (tell-app "Safari"
            "do Javascript \"document.title\" in front document"))

(defun my-safari-all-urls ()
  "Return a list of all the URLs in the front most Safari window."
  (split-string (do-applescript (concat "tell application \"Safari\"\n"
                                        "set links to \"\"\n"
                                        "repeat with t in every tab in front Window\n"
                                        "set links to links & the URL of t & linefeed\n"
                                        "end repeat\n"
                                        "return links\n"
                                        "end tell\n")) "\n" t))

(defun my-safari-all-urls-as-markdown ()
  (interactive)
  (let ((urls (my-safari-all-urls))
        (i 0))
    (dolist (url urls)
      (setq i (1+ i))
      (insert (format "[%d]: %s\n" i url)))))

(defun my-safari-url-as-markdown ()
  (interactive)
  (let ((url (my-safari-url))
        (title (my-safari-title)))
    (insert (concat "[" title "](" url ")"))))

(defun my-organisation ()
  "Return company name if I have one."
  (if (boundp 'my-company)
      (my-company)
    (user-full-name)))

(defun say-text (text)
  (do-applescript
   (concat "say \""
           text
           "\" waiting until completion false stopping current speech true")))

(defun speak-buffer-or-region ()
  "Read buffer or region aloud."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (say-text text))))

(defun stop-speech ()
  "Stopping talking."
  (interactive)
  (say-text ""))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(use-package deft
  :ensure t
  :bind ("<f5>" . deft)
  :config
  (setq deft-extension "org")
  (setq deft-directory "~/personal/notes")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.mdwn\\'" . markdown-mode)
         ("\\.markdown" . markdown-mode))
  :config
  (setq markdown-command "pandoc")
  (add-hook 'markdown-mode-hook 'imenu-add-menubar-index))


(use-package pd-blog-helpers
  :commands (pd-blog-draft
             pd-blog-publish-post))

(use-package writegood-mode
  :ensure t
  :commands (writegood-mode))

(use-package wc-mode
  :ensure t
  :commands (wc-mode))

(use-package bibtex
  :mode (("\\.bibtex\\'" . bibtex-mode)
         ("\\.bib\\'". bibtex-mode)))

(use-package ansi-term
  :bind ("<f10>" . pd-visit-term)
  :init
  (defun pd-visit-term ()
    ""
    (interactive)
    (if (not (get-buffer "*ansi-term*"))
        (ansi-term (getenv "SHELL"))
      (switch-to-buffer "*ansi-term*")))
  :config
  (defun my-term-paste (&optional string)
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (if string
         string
       (current-kill 0))))

  (defun my-term-hook ()
    (goto-address-mode))
  (add-hook 'term-mode-hook 'my-term-hook))

;; Setup for Org
(use-package org-agenda
  :bind (("<f6>" . my-org-agenda)
         ("C-c a" . org-agenda))
  :init
  (defun my-org-agenda ()
    (interactive)
    (org-agenda nil "w"))
  :config
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s %b")
          (timeline . "  % s %b")
          (todo . " %i %-12:c %b")
          (tags . " %i %-12:c %b")
          (search . " %i %-12:c %b"))))

(use-package org-mac-link
  :ensure t
  ;; :bind (:map org-mode-map
  ;;             ("C-c g" . org-mac-grab-link))
  :commands (org-mac-grab-link)
  :config
  (setq org-mac-grab-Addressbook-app-p nil)
  (setq org-mac-grab-devonthink-app-p nil)
  (setq org-mac-grab-Firefox-app-p nil)
  (setq org-mac-grab-Firefox+Vimperator-p nil)
  (setq org-mac-grab-Chrome-app-p nil)
  (setq org-mac-grab-Together-app-p nil)
  (setq org-mac-grab-Skim-app-p nil))

(use-package org-capture
  :bind (("C-c r" . org-capture))
  :config
  (setq org-capture-templates
        '(("i" "Interruption" entry
           (file "~/work/org/inbox.org")
           "* %?\n"
           :clock-in t)
          ("n" "Notes" entry
           (file "~/work/org/inbox.org")
           "* %?\n%U\n%i\n%a")
          ("t" "Todo" entry
           (file "~/work/org/inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ("b" "Book" entry
           (file+headline "~/personal/notes/reading.org" "Read")
           "** %^{Title}\n:PROPERTIES:\n:Author: %^{Author}p \n:Started: %u\n:Finished: \n:END:\n\n"
           :immediate-finish t))))

(use-package ox-bibtex
  :defer t
  :load-path "vendor/")

(use-package htmlize
  :ensure t
  :defer t)

(use-package ox-html
  :defer t
  :config
  (progn
    (require 'htmlize)
    (setq org-html-htmlize-output-type 'css)))

(use-package ox-publish
  :defer t
  :commands pd/publish-blog
  :config
  (require 'ox-html)
  (require 'pd-html)
  (require 'ox-rss)

  (defun pd/publish-blog ()
    "Publish my blog"
    (interactive)
    (org-publish-project "blog" t))

  (setq org-confirm-babel-evaluate nil)

  (setq org-publish-project-alist
        '(("blog-content"
           :base-directory "~/personal/phil.dixon.gen.nz/"
           :base-extension "org"
           :recursive t
           :publishing-directory "~/Sites/phil.dixon.gen.nz/"
           :publishing-function (pd-html-publish-to-html)
           :with-toc nil
           :html-html5-fancy t
           :section-numbers nil
           :exclude "rss.org")
          ("blog-static"
           :base-directory "~/personal/phil.dixon.gen.nz/"
           :base-extension "jpg\\|png\\|css\\|js\\|ico\\|gif"
           :recursive t
           :publishing-directory "~/Sites/phil.dixon.gen.nz/"
           :publishing-function org-publish-attachment)
          ("blog-rss"
           :base-directory "~/personal/phil.dixon.gen.nz/"
           :base-extension "org"
           :publishing-directory "~/Sites/phil.dixon.gen.nz/"
           :publishing-function (org-rss-publish-to-rss)
           :html-link-home "~/Sites/phil.dixon.gen.nz/"
           :html-link-use-abs-url t
           :exclude ".*"
           :include ("rss.org")
           :with-toc nil
           :section-numbers nil
           :title "Phillip Dixon")
          ("blog"
           :components
           ("blog-content" "blog-static" "blog-rss")))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-directory "~/work/org/")
  (setq org-default-notes-file (concat org-directory "inbox.org"))
  (setq org-agenda-diary-file (concat org-directory "diary.org"))
  (setq org-agenda-files (list org-directory (concat org-directory "projects/")))

  (setq org-hide-leading-stars t)
  (setq org-use-sub-superscripts "{}")
  (setq org-footnote-define-inline t)
  (setq org-footnote-auto-adjust nil)

  (setq org-fast-tag-selection-single-key 'expert)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-tag-alist
        '((:startgroup . nil)
          ("@call" . ?c)
          ("@office" . ?o)
          ("@home" . ?h)
          ("@read" . ?r)
          ("@computer" . ?m)
          ("@shops" . ?s)
          ("@dev" . ?d)
          ("@write" . ?w)
          (:endgroup . nil)
          ("REFILE" . ?f)
          ("SOMEDAY" . ?s)
          ("PROJECT" . ?p)))
  (setq org-use-tag-inheritance t)
  (setq org-tags-exclude-from-inheritance '("@call"
                                            "@office"
                                            "@home"
                                            "@read"
                                            "@computer"
                                            "@shops"
                                            "@dev"
                                            "@write"
                                            "PROJECT"))

  (setq org-use-speed-commands t)
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "|" "CANCELLED" "DELEGATED(e@)")))
  (setq org-enforce-todo-dependencies t)
  (defun pd/org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'pd/org-summary-todo)

  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
        '(("w" "Day's Agenda and Tasks"
           ((agenda "" (( org-agenda-span 1)))
            (tags-todo "-SOMEDAY/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
            (tags-todo "-SOMEDAY/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'bh/skip-non-projects)
                        (org-agenda-ignore-scheduled 'future)
                        (org-agenda-ignore-deadlines 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED/!WAITING"
                       ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                        (org-agenda-skip-function 'pd/skip-projects)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)))
            (tags-todo "-SOMEDAY/!-WAITING"
                       ((org-agenda-overriding-header "Tasks")
                        (org-agenda-skip-function 'pd/skip-projects)
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-todo-ignore-deadlines t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            nil))
          ("#" "Stuck Projects" tags-todo "-SOMEDAY/!"
           ((org-agenda-overriding-header "Stuck Projects")
            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
          ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-WAITING"
           ((org-agenda-overriding-header "Tasks")
            (org-agenda-skip-function 'pd/skip-projects)
            (org-agenda-sorting-strategy
             '(category-keep))))
          ("p" "Project Lists" tags-todo "-SOMEDAY/!"
           ((org-agenda-overriding-header "Projects")
            (org-agenda-skip-function 'bh/skip-non-projects)
            (org-agenda-ignore-scheduled 'future)
            (org-agenda-ignore-deadlines 'future)
            (org-agenda-sorting-strategy
             '(category-keep))))
          ("b" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING"
           ((org-agenda-overriding-header "Waiting and Postponed tasks")
            (org-agenda-skip-function 'pd/skip-projects)
            (org-agenda-todo-ignore-scheduled 'future)
            (org-agenda-todo-ignore-deadlines 'future)))
          ("e" "Errand List" tags-todo "@shops"
           ((org-agenda-prefix-format "[ ]")
            (org-agenda-todo-keyword-format "")))
          ("c" todo "TODO"
           ((org-agenda-sorting-strategy '(tag-up priority-down))))))

  (add-hook 'org-agenda-mode-hook '(lambda ()
                                     (setq org-agenda-tags-column (- (window-width)))))

    ;; Refile setup

  (setq org-completion-use-ido t)
  (setq org-refile-targets (quote ((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3))))
  (setq org-refile-use-outline-path (quote file))
  (setq org-outline-path-complete-in-steps t)

  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun bh/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next (save-excursion
                               (forward-line 1)
                               (and (< (point) subtree-end)
                                    (re-search-forward "^\\*+ \\(TODO\\) " subtree-end t)))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    (bh/list-sublevels-for-projects-indented)
    (if (save-excursion (bh/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (if (bh/is-project-p)
                nil
              subtree-end)))
      (org-end-of-subtree t)))

  (defun pd/skip-projects ()
    "Skip trees that are projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-project-p)
          next-headline)
         (t
          nil))))))

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-expand yas-hippie-try-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (eval-after-load 'text-mode
    (add-hook 'text-mode-hook 'yas-minor-mode))
  (eval-after-load 'prog-mode
    (add-hook 'prog-mode-hook 'yas-minor-mode))
  :config
  (setq yas-snippet-dirs (list (concat dotfiles-dir "snippets")))
  (setq yas-prompt-functions '(yas-ido-prompt yas-complete-prompt))
  (yas-reload-all))

(use-package hippie-exp
  :defer t
  :config
  ;; Hippie expand: at times perhaps too hip
  (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
    (delete f hippie-expand-try-functions-list))

  ;; Add this back in at the end of the list.
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t))

(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'company-mode)
  (add-hook 'elisp-mode 'company-mode)
  :config
  (require 'company-irony)
  (add-to-list 'company-backends 'company-irony)
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-begin-commands '(self-insert-command))
  (setq company-idle-delay 0.3))

(use-package s
  :defer t
  :ensure t)

(use-package autoinsert
  :defer t
  :preface
  (defun pd-expand-by-uuid (mode uuid)
    "Expand snippet template in MODE by its UUID"
    (let ((template (yas--get-template-by-uuid mode uuid)))
      (yas-expand-snippet
       (yas--template-content template)
       nil
       nil
       (yas--template-expand-env template))))
  (defun pd-expand-buffer ()
    "Expand buffer in place as a yasnippet."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  :init
  (add-hook 'find-file-hooks 'auto-insert)
  :config
  (setq auto-insert-directory (concat dotfiles-dir "mytemplates/")
        auto-insert-query nil)

  (define-auto-insert "setup.py\\'"
    ["template-setup.py" pd-expand-buffer])

  (define-auto-insert "\\.markdown\\'"
    ["post.markdown" pd-expand-buffer])

  (define-auto-insert "\\.mdwn\\'"
    ["template.mdwn" pd-expand-buffer])

  (define-auto-insert "\\.m\\'"
    ["template.m" pd-expand-buffer])

  (define-auto-insert "\\.org\\'"
    #'(lambda () (pd-expand-by-uuid 'org-mode "header.yasnippet"))))


(use-package haskell-mode
  :disabled t
  :ensure t
  :mode ("\\.l?hs\\'" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter (("lua" . lua-mode)
                ("luajit" . lua-mode))
  :config
  (setq lua-indent-level 4))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter (("python" . python-mode)
                ("python3" . python-mode))
  :config
  (defun pd-python-mode-hook ()
    (electric-indent-mode -1)) ;; This isn't useful in python

  (add-hook 'python-mode-hook 'pd-python-mode-hook))

(use-package wizard-db
  :mode ("\\.xmd\\'" . wizard-db-mode))

(use-package lilypond-mode
  :load-path "vendor/lilypond"
  :mode ("\\.ly\\'" . LilyPond-mode))

(use-package pkgbuild-mode
  :ensure t
  :mode ("PKGBUILD\\'" . pkgbuild-mode))

(use-package conf-mode
  :mode ("hgrc" . conf-mode))

(use-package dummy-h-mode
  :ensure t
  :mode ("\\.h$" . dummy-h-mode))

(use-package cc-mode
  :defer t
  :config
  (use-package google-c-style
    :ensure t
    :init
    (c-add-style "Google" google-c-style)

    (defconst my-c-style
      '("Google"
        (c-basic-offset . 4)
        (c-offsets-alist . ((inextern-lang . -)))))
    (c-add-style "PERSONAL" my-c-style)

    (defconst dcl-c-style
      '("Google"
        (c-basic-offset . 3)))
    (c-add-style "DCL" dcl-c-style))

  (defconst my-obj-c-style
    '("bsd"
      (c-basic-offset . 4)
      (indent-tabs-mode . nil)
      (c-offsets-alist . ((case-label . +)))))
  (c-add-style "my-obj-c" my-obj-c-style)

    ;; Customizations for all modes in CC Mode.

  (defun my-c-mode-common-hook ()
    (c-set-style "PERSONAL")
    (setq ff-always-in-other-window nil))

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

  (defun pd/objc-ff-setup-hook ()
    (set (make-local-variable 'cc-other-file-alist)
         '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp")))))

  (add-hook 'objc-mode-hook 'pd/objc-ff-setup-hook)

  (use-package pd-cc-mode-extras
    :commands (pd/toggle-header
               pd/toggle-test)))

(use-package compile
  :defer t
  :config
  (defun pd/compilation-hook ()
    (setq truncate-lines t))

  (add-hook 'compilation-mode-hook 'pd/compilation-hook))

(use-package eldoc
  :diminish eldoc-mode
  :defer t)

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :defer t)

(use-package lisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :defer t
  :init
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package flycheck-irony
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

;; (use-package switch-window
;;   :disabled t
;;   :ensure t
;;   :bind ("C-x o" . switch-window))

(use-package popwin
  :ensure t
  :disabled t
  :init
  (popwin-mode t))

;; From emacs start kit v2.
;;; These belong in prog-mode-hook:

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.

(defun pd/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun pd/turn-on-hl-line-mode ()
  (when window-system (hl-line-mode t)))

(defun pd/turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun pd/turn-on-whitespace ()
  (whitespace-mode t))

(defun pd/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|FIX\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(defun pd/turn-on-which-func ()
  (which-function-mode t))

(add-hook 'prog-mode-hook 'pd/local-comment-auto-fill)
(add-hook 'prog-mode-hook 'pd/turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'pd/turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'pd/turn-on-whitespace)
(add-hook 'prog-mode-hook 'pd/add-watchwords)
(add-hook 'prog-mode-hook 'pd/turn-on-which-func)

(use-package server
  :defer t)

(use-package mu4e
  :defer t
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :config
  (require 'mu4e-contrib)
  (setq mu4e-maildir "~/.mail/dixon.gen.nz"
        mu4e-view-prefer-html t
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-use-fancy-chars t))

(use-package clang-format
  :defer t
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (setq clang-format-executable "/usr/local/opt/llvm36/bin/clang-format-3.6")))

(use-package info
  :defer t
  :config
  (set-face-attribute 'Info-quoted nil
                      :family 'unspecified
                      :inherit font-lock-type-face))

(use-package mediawiki
  :ensure t
  :defer t
  :config
  (add-to-list 'mediawiki-site-alist
                 '("Software" "http://wiki.sw.au.ivc/mediawiki" "pdixon" "" "The PENSIEVE"))
  (setq mediawiki-site-default "Software"))


(if (file-exists-p system-specific-config)
    (load system-specific-config)
  (message "No system specific config for %s (i.e %s doesn't exist)"
           (system-name)
           system-specific-config))

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))
;;; init.el ends here
