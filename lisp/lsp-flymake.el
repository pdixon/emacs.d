;;; lsp-flymake.el --- lsp based flymake back-end   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'lsp-mode)
(require 'lsp-notifications)
(require 'flymake)

;; from http://emacs.stackexchange.com/questions/8082/how-to-get-buffer-position-given-line-number-and-column-number
(defun lsp-flymake--position-to-point (params)
  "Convert Position object in PARAMS to a point."
  (save-excursion
    (goto-char (point-min))
    (forward-line (plist-get params :line))
    (forward-char (plist-get params :column))
    (point)))

(defun lsp--diagnostic-to-flymake (file diag)
  (let* ((range (lsp-diagnostic-range diag))
                         (begin (lsp-flymake--position-to-point (plist-get range :start)))
                         (end (lsp-flymake--position-to-point (plist-get range :end)))
                         (type (pcase (lsp-diagnostic-severity diag)
                                  (1 :error)
                                  (2 :warning)
                                  (_ :note)))
                         (msg (lsp-diagnostic-message diag)))
    (flymake-make-diagnostic
     (current-buffer) ;; FIXME What if the diagnostic isn't for this file
     begin
     end
     type
     msg)))

(defun lsp-flymake (report-fn &rest _args)
  (let ((errors))
    (maphash (lambda (file diagnostics)
               (dolist (diag diagnostics)
                 (push
                  (lsp--diagnostic-to-flymake file diag)
                  errors)))
             lsp--diagnostics)
    (funcall report-fn errors)))

(defun lsp-setup-flymake-backend ()
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (add-hook 'flymake-diagnostic-functions 'lsp-flymake nil t)
  (add-hook 'lsp-after-diagnostics-hook 'flymake-start nil t))

(provide 'lsp-flymake)
;;; lsp-flymake.el ends here
