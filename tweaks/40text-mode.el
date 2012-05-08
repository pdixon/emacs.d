;; Customisation for text mode.
(add-hook 'text-mode-hook
	  '(lambda ()
		  (auto-fill-mode 1)
		  (flyspell-mode 1)))
