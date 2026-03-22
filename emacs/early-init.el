;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Code:

(setq package-enable-at-startup nil)

;; Ensure ELPA transient takes precedence over Emacs 30's bundled version
;; (transient-define-group was added after the version Emacs 30 ships with)
(when-let ((dir (car (sort (file-expand-wildcards
                            (expand-file-name "elpa/transient-*" user-emacs-directory))
                           #'string>))))
  (push dir load-path))

;; Disable UI elements early to avoid flicker
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; early-init.el ends here
