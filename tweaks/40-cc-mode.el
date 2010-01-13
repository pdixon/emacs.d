;; Create my personal C style.

(require 'google-c-style)
(c-add-style "Google" google-c-style)

(defconst my-c-style
'("Google"
  (c-basic-offset . 4)))
(c-add-style "PERSONAL" my-c-style)

(defconst dcl-c-style
'("Google"
  (c-basic-offset . 3)))
(c-add-style "DCL" dcl-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (c-set-style "PERSONAL"))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;; Xcode support from http://han9kin.doesntexist.com/29
(defvar xcode-compile-sdks nil)
(defvar xcode-compile-sdk-history nil)

(dolist (line
         (split-string
          (with-temp-buffer
            (call-process "xcodebuild" nil t nil "-showsdks")
            (buffer-string))
          "\n" t))
  (let ((comps (split-string line "-sdk " t)))
    (if (> (length comps) 1)
        (add-to-list 'xcode-compile-sdks (car (last comps))))))

(defun xcode-compile ()
  (interactive)
  (let ((command "xcodebuild -activeconfiguration -activetarget"))
    (setq command
          (concat
           command
           (if xcode-compile-sdks
               (let ((default-sdk (or (car xcode-compile-sdk-history) (car xcode-compile-sdks))))
                 (concat
                  " -sdk "
                  (completing-read
                   (format "Compile with sdk (default %s): " default-sdk)
                   xcode-compile-sdks
                   nil
                   t
                   nil
                   'xcode-compile-sdk-history
                   default-sdk))))
           (let ((dir ".") (files nil))
             (while
                 (progn
                   (setq files (directory-files dir nil "\\.xcodeproj\\'"))
                   (and (not (string-equal "/" (expand-file-name dir))) (null files)))
               (setq dir (concat (file-name-as-directory dir) "..")))
             (unless (null files) (concat " -project " (file-name-as-directory dir) (car files))))))
    (compile (read-string "Compile command: " (concat command " ")))))
