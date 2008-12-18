(add-to-list 'load-path "~/elisp")

(show-paren-mode 1)
(setq-default x-stretch-cursor t)
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'sentence-end-double-space nil)
(server-start)

(defalias 'list-buffers 'ibuffer)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(require 'mercurial)
(require 'outline-magic)

;; Setup for better printing
(require 'printing)
(pr-update-menus)

;; Setup for ido.el
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Setup for Org
;(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Setup for Org Remember
(require 'remember)
(org-remember-insinuate)
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "master.org"))
(define-key global-map "\C-cr" 'org-remember)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n  %a" "master.org" "Tasks")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "master.org" "Ideas")))

;; Setup for xgtags
(require 'xgtags)

;; Setup for eshell mode

;; Setup for yasnippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/snippets/")

(add-hook 'org-mode-hook
          '(lambda ()
             (make-variable-buffer-local 'yas/trigger-key)
             (setq yas/trigger-key [tab])))

;; mail setup
(require 'smtpmail)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil)))
(setq smtpmail-auth-credentials
      '(("smtp.gmail.com"
	 587
	 "phillip.dixon@gmail.com"
	 nil)))
(setq message-send-mail-function 'smtpmail-send-it)
(setq send-mail-function 'smtpmail-send-it)

;; Project Root setup
(require 'project-root)
(setq project-roots
      '(("Hg Hosted Project"
	 :root-contains-files (".hg")
	 :on-hit (lambda (p) (message (car p))))))

(defun anything-project-root-find-files (pattern)
  (when anything-project-root
      (start-process-shell-command "project-root-find"
                                   nil
                                   "find"
                                   anything-project-root
                                   (find-to-string
                                    `(and (prune (name "*.svn" "*.git" "*.hg"))
                                          (name ,(concat "*" pattern "*"))
                                          (type "f"))))))

(defvar anything-c-source-project-files
  '((name . "Project Files")
    (init . (lambda ()
              (unless project-details (project-root-fetch))
              (setq anything-project-root (cdr project-details))))
    (candidates . (lambda ()
                    (anything-project-root-find-files anything-pattern)))
    (type . file)
    (requires-pattern . 2)
    (volatile)
    (delayed)))

(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))


(require 'anything)
(require 'anything-config)
(require 'anything-make)
(setq anything-sources
      (list anything-c-source-buffers
	   ; anything-c-source-filename-history
	   ; anything-c-source-file-cache
	    anything-c-source-make-targets
	    anything-c-source-project-files
	    anything-c-source-occur
	    anything-c-source-info-pages
	    anything-c-source-man-pages
	    anything-c-source-emacs-commands))
(global-set-key (kbd "M-X") 'anything)

;; Create my personal C style.
(defconst my-c-style
  '((c-basic-offset . 3)
    (c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 3)
                                   (block-open        . 0)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")
(c-add-style "PERSONAL" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (xgtags-mode 1)
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq tab-width 3
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Customisation for text mode.
(add-hook 'text-mode-hook
	  '(lambda ()
		  (auto-fill-mode 1)
		  (flyspell-mode 1)))

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

;; key bindings
(load-file "~/elisp/ergonomic_keybinding_dvorak.el")
;(global-set-key [(control .)] 'tags-search) ;Regex through files in tag table.
;(global-set-key "\C-w" 'backward-kill-word)
;(global-set-key "\C-x\C-k" 'kill-region)
;(global-set-key [(control *)] 'my-search-word-forward)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(org-agenda-files (quote ("~/org/master.org")))
 '(rst-adornment-faces-alist nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(require 'zenburn)
(unless (zenburn-format-spec-works-p)
  (zenburn-define-format-spec))
(require 'color-theme)
(color-theme-initialize)
(color-theme-zenburn)

