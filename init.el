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

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Basic Config...done (%.3fs)" elapsed))

;; package.el setup
(require 'package)

(defun require-package (package)
  "Ask elpa to install given PACKAGE."
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)
(setq package-enable-at-startup nil)
(require-package 'use-package)
(require 'ert) ; FIXME 2014-02-13 remove once upstream use-package unbreaks.
(require 'use-package)
(setq use-package-verbose t)

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Package Config...done (%.3fs)" elapsed))

(require-package 'applescript-mode)
(require-package 'auctex)
(require-package 'cmake-mode)
(require-package 'diminish)
(require-package 'git-commit-mode)
(require-package 'gitconfig-mode)
(require-package 'gitignore-mode)
(require-package 'go-mode)
(require-package 'graphviz-dot-mode)
(require-package 'ibuffer-vc)
(require-package 'window-number)
(require-package 'zenburn-theme)

;; Basic Apperance
;; (if (not (eq system-type 'darwin))
;;     (menu-bar-mode 0))
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
(use-package recentf
  :init
  (recentf-mode 1))

(use-package autorevert
  :init (global-auto-revert-mode)
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t
          auto-revert-check-vc-info t
          auto-revert-verbose nil)))

(electric-pair-mode 1)
;;(electric-indent-mode 1)
;;(electric-layout-mode 1)

(use-package savehist
  :init (savehist-mode t))

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

(when window-system
  (setq frame-title-format '(buffer-file-name "emacs - %f" ("emacs - %b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Apperance
(setq solarized-distinct-fringe-background t
      solarized-use-variable-pitch nil
      solarized-height-minus-1 1.0
      solarized-height-plus-1 1.0
      solarized-height-plus-2 1.0
      solarized-height-plus-3 1.0
      solarized-height-plus-4 1.0)

(setq vc-handled-backends '(Git Hg))
(setq whitespace-style '(face trailing tabs)
      whitespace-line-column 80)



(setq split-height-threshold nil
      split-width-threshold nil)

(setq message-kill-buffer-on-exit t)
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "phil@dixon.gen.nz")
(setq user-full-name "Phillip Dixon")

(use-package eudc
  :defer t
  :config
  (progn
    (eudc-set-server "localhost" 'mab t)
    (eudc-protocol-set 'eudc-inline-expansion-format
                       '("%s %s <%s>" firstname lastname email)
                       'mab)

    (defun eudc-select (choices beg end)
      (let ((replacement
             (ido-completing-read "Multiple matches found; choose one: "
                                  (mapcar 'list choices))))
        (delete-region beg end)
        (insert replacement)))))

(use-package message
  :defer t
  :config
  (progn
    (require 'eudc)))

(setq send-mail-function 'smtpmail-send-it 
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "phil@dixon.gen.nz" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq ispell-dictionary "en_GB-ise"
      ispell-extra-args `("--keyboard=dvorak"))

(use-package imenu
  :defer t
  :config
  (progn
    (setq imenu-max-items 200)))

(use-package ibuffer
  :defer t
  :bind ("<f8>" . ibuffer)
  :init
  (progn
    (defalias 'list-buffers 'ibuffer))
  :config
  (progn
    (setq ibuffer-expert 1)
    (setq ibuffer-show-empty-filter-groups nil)

    (defun pd/dired-do-multi-occur (regexp)
      "Show all in lines in marked files containing REGEXP"
      (interactive "MList lines matching regexp: ")
      (multi-occur (mapcar 'find-file (dired-get-marked-files)) regexp))

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
                  filename-and-process)))))


(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'reverse
          uniquify-separator "/"
          uniquify-after-kill-buffer-p t
          uniquify-ignore-buffers-re "^\\*")))

(use-package dired
  :defer t
  :config
  (progn
    (setq dired-listing-switches "-alh --group-directories-first")
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-dwim-target t
          dired-recursive-copies 'always
          dired-recursive-deletes 'top
          dired-listing-switches "-alh")
    (defun pd-dired-find-alternate-parent ()
      (interactive)
      (find-alternate-file ".."))
    (bind-key "^" 'pd-dired-find-alternate-parent dired-mode-map)))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window)))

(defun pd/light ()
  "Activate light theme."
  (interactive)
  (load-theme 'solarized-light t t)
  (load-theme 'pd-basic t t)
  (custom-set-variables '(custom-enabled-themes '(pd-basic solarized-light))))

(defun pd/dark ()
  "Activate dark theme."
  (interactive)
  (load-theme 'solarized-dark t t)
  (load-theme 'pd-basic t t)
  (custom-set-variables '(custom-enabled-themes '(pd-basic solarized-dark))))

(defun pd/zenburn ()
  "Activate dark theme."
  (interactive)
  (load-theme 'zenburn t t)
  (load-theme 'pd-basic t t)
  (custom-set-variables '(custom-enabled-themes '(pd-basic zenburn))))


;; make scripts executable on save.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


(use-package expand-region
  :ensure t
  :bind (("M-\"" . er/contract-region)
         ("M-'" . er/expand-region)))

(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package magit
  :ensure t
  :bind ("<f7>" . magit-status)
  :config
  (progn
    (setq magit-status-buffer-switch-function 'switch-to-buffer
          magit-diff-refine-hunk t)
    (add-hook 'magit-log-edit-mode-hook
              #'(lambda ()
                  (set-fill-column 72)
                  (flyspell-mode)))))

(use-package hg-commit-mode
  :mode ("hg-editor-.*\\.txt\\'" . hg-commit-mode))

(use-package ediff
  :defer t
  :config
  (progn
    (setq ediff-split-window-function 'split-window-horizontally)))

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

;; Yank line or region
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

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
  (progn
    (require 'pd-project)
    (setq ffip-project-root-function 'pd-project-get-root)))

(use-package pd-project
  :bind(("C-c b" . pd-project-compile))
  :commands (pd-project-grep
             pd-project-todo))

(use-package pd-window-extras
  :commands (pd/rotate-windows
             pd/toggle-window-split
             pd/setup-windows
             pd/toggle-just-one-window))


(defun my-toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))
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
  "Copy lines (as many as prefix argument) in the kill ring"
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
  "Use Applescript to tell an Application"
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
  (let ((url my-safari-url)
        (title my-safari-title))
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
  "Reads a buffer or region aloud."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (say-text text))))

(defun stop-speech ()
  "stopping talking."
  (interactive)
  (say-text ""))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(add-hook 'text-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode 1)))

(use-package deft
  :ensure t
  :bind ("<f5>" . deft)
  :config
  (progn
    (setq deft-extension "org")
    (setq deft-directory "~/personal/notes")
    (setq deft-text-mode 'org-mode)
    (setq deft-use-filename-as-title t)))

(use-package markdown-mode
  :ensure t
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

;; Setup for Org
(use-package org-agenda
  :bind (("<f6>" . my-org-agenda)
         ("C-c a" . org-agenda))
  :init
  (progn
    (defun my-org-agenda ()
      (interactive)
      (org-agenda nil "w")))
  :config
  (progn
    (setq org-agenda-prefix-format
          '((agenda . " %i %-12:c%?-12t% s %b")
            (timeline . "  % s %b")
            (todo . " %i %-12:c %b")
            (tags . " %i %-12:c %b")
            (search . " %i %-12:c %b")))))

(use-package org-capture
  :bind (("C-c r" . org-capture)))

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
  (progn
    (require 'ox-html)
    (require 'pd-html)

    (defun pd/publish-blog ()
      "Publish my blog"
      (interactive)
      (org-publish-remove-all-timestamps)
      (org-publish-project "blog"))

    (setq org-confirm-babel-evaluate nil)

    (setq org-publish-project-alist
          '(("blog-content"
             :base-directory "~/personal/phil.dixon.gen.nz/"
             :base-extension "org"
             :recursive t
             :publishing-directory "~/Sites/phil.dixon.gen.nz/"
             :publishing-function pd-html-publish-to-html
             :with-toc nil
             :section-numbers nil
             :html-html5-fancy t)
            ("blog-static"
             :base-directory "~/personal/phil.dixon.gen.nz/"
             :base-extension "jpg\\|png\\|css\\|js\\|ico\\|gif"
             :recursive t
             :publishing-directory "~/Sites/phil.dixon.gen.nz/"
             :publishing-function org-publish-attachment)
            ("blog"
             :components
             ("blog-content" "blog-static"))))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (progn
            (setq org-directory "~/work/org/")
            (setq org-default-notes-file (concat org-directory "inbox.org"))
            (setq org-agenda-diary-file (concat org-directory "diary.org"))
            (setq org-agenda-files (list org-directory (concat org-directory "projects/")))

            (setq org-hide-leading-stars t)
            (setq org-use-sub-superscripts "{}")

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


            (setq org-use-fast-todo-selection t)
            (setq org-todo-keywords
                  '((sequence "TODO(t)" "|" "DONE(d!)")
                    (sequence "WAITING(w@/!)" "|" "CANCELLED" "DELEGATED(e@)")))
            (setq org-enforce-todo-depedencies t)
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
                    ("w"
                     "Default template"
                     entry
                     (file "~/work/org/inbox.org")
                     "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
                     :empty-lines 1)))

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
                    nil)))))))


(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(setq tab-always-indent 'complete)

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode yas-expand yas-hippie-try-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (hook-into-modes #'(lambda () (yas-minor-mode 1))
                   '(prog-mode-hook
                     org-mode-hook
                     message-mode-hook
                     markdown-mode-hook))
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt yas-complete-prompt))
    (setq yas-snippet-dirs (list (concat dotfiles-dir "snippets")))
    (yas-reload-all)))

(use-package hippie-exp
  :defer t
  :config
  (progn
    ;; Hippie expand: at times perhaps too hip
    (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
      (delete f hippie-expand-try-functions-list))

    ;; Add this back in at the end of the list.
    (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)))

(defvar xcode:sdkver "7.0")
(defvar xcode:sdkpath "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer")
(defvar xcode:sdk (concat xcode:sdkpath "/SDKs/iPhoneSimulator" xcode:sdkver ".sdk"))

(use-package company
  :ensure t
  :defer t
  :config
  (progn
    (setq company-begin-commands '(self-insert-command))
    (setq company-idle-delay 0.3)

    (setq company-clang-arguments nil)
    (add-to-list 'company-clang-arguments "-fobjc-arc" t)
    (add-to-list 'company-clang-arguments "-fblocks" t)
    (add-to-list 'company-clang-arguments "-isysroot" t)
    (add-to-list 'company-clang-arguments xcode:sdk t)
    ;(add-to-list 'company-clang-arguments "-D__IPHONE_OS_VERSION_MIN_REQUIRED=60000" t)
    ))

(use-package autoinsert
  :defer t
  :init
  (add-hook 'find-file-hooks 'auto-insert)
  :config
  (progn
    (setq auto-insert-directory (concat dotfiles-dir "mytemplates/")
          auto-insert-query nil)

    (defun pd-expand-buffer ()
      "Expand buffer in place as a yasnippet."
      (yas-expand-snippet (buffer-string) (point-min) (point-max)))

    (define-auto-insert "\\.markdown\\'"
      ["post.markdown" pd-expand-buffer])

    (define-auto-insert "\\.mdwn\\'"
      ["template.mdwn" pd-expand-buffer])

    (define-auto-insert "\\.m\\'"
      ["template.m" pd-expand-buffer])))

(use-package haskell-mode
  :ensure t
  :mode ("\\.l?hs\\'" . haskell-mode)
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter (("lua" . lua-mode)
                ("luajit" . lua-mode))
  :config
  (progn
    (setq lua-indent-level 4)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter (("python" . python-mode)
                ("python3" . python-mode))
  :config
  (progn
    (defun pd-python-mode-hook ()
      (electric-indent-mode -1) ;; This isn't useful in python
      )

    (add-hook 'python-mode-hook 'pd-python-mode-hook)))

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
  (progn
    (use-package google-c-style
      :ensure t
      :init
      (progn
        (c-add-style "Google" google-c-style)

        (defconst my-c-style
          '("Google"
            (c-basic-offset . 4)
            (c-offsets-alist . ((inextern-lang . -)))))
        (c-add-style "PERSONAL" my-c-style)

        (defconst dcl-c-style
          '("Google"
            (c-basic-offset . 3)))
        (c-add-style "DCL" dcl-c-style)))

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
    ;;(add-hook 'objc-mode-hook 'company-clang)

    (use-package pd-cc-mode-extras
      :commands (pd/toggle-header
                 pd/toggle-test))))

(use-package compile
  :defer t
  :config
  (progn
    (defun pd/compilation-hook ()
      (setq truncate-lines t))

    (add-hook 'compilation-mode-hook 'pd/compilation-hook)))

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
  (progn
    (defun pd/elisp-mode-hook ()
      (elisp-slime-nav-mode)
      (eldoc-mode))

    (add-hook 'emacs-lisp-mode-hook 'pd/elisp-mode-hook)))

(use-package flycheck
  :ensure t
  :defer t)

(use-package switch-window
  :ensure t
  :bind ("C-x o" . switch-window))

(use-package popwin
  :ensure t
  :init
  (progn
    (popwin-mode t)))

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


(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package server
  :init (server-start))

(pd/zenburn)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'none)

(if (eq system-type 'darwin)
    (setq insert-directory-program "gls"))

(if (file-exists-p system-specific-config)
    (load system-specific-config)
  (message "No system specific config for %s (i.e %s doesn't exist)"
           system-name
           system-specific-config))

(let ((elapsed (float-time (time-subtract (current-time)
                                          *emacs-load-start*))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))
;;; init.el ends here
