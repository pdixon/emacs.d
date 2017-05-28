;;; lsp-swift.el --- Langauge Server support for Swift  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Package-Requires ((lsp-mode) (swift-mode))
;; Keywords: swift

;;; Commentary:

;;; Code:

(require 'lsp-mode)

(defun lsp-swift--get-root ()
  (or (expand-file-name (locate-dominating-file default-directory "Package.swift"))
      (user-error "Couldn't find swift project")))

;;;###autoload
(lsp-define-stdio-client 'swift-mode "swift" 'stdio
                         #'lsp-swift--get-root
                         "Swift Language Server"
                         '("~/mess/2017/10/langserver-swift/.build/debug/langserver-swift"))

(provide 'lsp-swift)

;;; lsp-swift.el ends here
