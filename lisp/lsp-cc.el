;;; lsp-cc.el --- Langauge Server support for c, c++, objc  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Phillip Dixon

;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Keywords: c,

;;; Commentary:

;;; Code:

(require 'lsp-mode)

(defun lsp-cc--get-root ()
  (nth 0 (project-roots (project-current t))))

;;;###autoload
(lsp-define-stdio-client lsp-clangd
                         "clangd"
                         #'lsp-cc--get-root
                         '("/usr/local/opt/llvm/bin/clangd")
                         :ignore-regexps '("^Notification ignored.$"))

(provide 'lsp-cc)
;;; lsp-cc.el ends here
