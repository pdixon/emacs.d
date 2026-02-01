;;; org-lotus-notes.el -- Links to Lotus Notes Documents from within Org-mode

;; Copyright (C) 2009 Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>

;;; Commentary:
;; 

;;; Code:

(require 'org)

(org-add-link-type "notes" 'org-lotus-notes-open)
(org-add-link-type "Notes" 'org-lotus-notes-open)


(defun org-lotus-notes-open (doc-id)
  "Visit the document with the given DOC-ID."
  (start-process "notes" nil "/usr/local/bin/notes" (concat "Notes:" doc-id)))


(provide 'org-lotus-notes)

;;; org-lotus-notes.el ends here


