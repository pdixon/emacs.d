;;; lsp-python.el --- Python Language Server support  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Author: Phillip Dixon <phil@dixon.gen.nz>
;; Package-Requires: ((lsp-mode "3.0"))
;; Keywords: python

;; Code:
(require 'lsp-mode)
(require 'lsp-common)

(lsp-define-stdio-client lsp-python "python"
                         (lsp-make-traverser "setup.py")
                         '("pyls"))

(provide 'lsp-python)
;;; lsp-python.el ends here
