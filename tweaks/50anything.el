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
	    anything-c-source-make-targets
	    anything-c-source-project-files
	    anything-c-source-occur
	    anything-c-source-info-pages
	    anything-c-source-man-pages
	    anything-c-source-emacs-commands))
