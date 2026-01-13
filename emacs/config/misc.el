;;; misc.el --- Miscellaneous packages and utilities -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains miscellaneous packages and utilities:
;; - Helpful: Better help buffers
;; - Command log mode: Display commands as you type
;; - Restart Emacs: Convenient restart functionality
;; - Joplin mode: Support for Joplin notes
;; - YAML mode: YAML file support

;;; Code:

;; =============================================================================
;; Helpful (Enhanced Help Buffers)
;; =============================================================================

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(defun describe-thing-at-point ()
  "Show the documentation of the symbol at point."
  (interactive)
  (let ((thing (symbol-at-point)))
    (if thing
        (describe-symbol thing)
      (message "No symbol at point."))))

(global-set-key (kbd "C-c d") 'describe-thing-at-point)

;; =============================================================================
;; Command Log Mode
;; =============================================================================

(use-package command-log-mode
  :ensure t)

;; =============================================================================
;; Restart Emacs
;; =============================================================================

(use-package restart-emacs
  :ensure t
  :bind ("C-c x r" . restart-emacs))

;; =============================================================================
;; Joplin Mode
;; =============================================================================

;; Download joplin-mode.el from:
;; https://discourse.joplinapp.org/t/note-for-emacs-users/623

;; Load the major mode file
(load-file "~/.emacs.d/lisp/joplin-mode.el")

;; Declare and autoload the mode
(autoload 'joplin-mode "joplin-mode"
  "Major mode for editing Joplin files" t)

;; Note that joplin-mode will step down if it is not joplin data
(add-to-list 'auto-mode-alist '("/[a-f0-9]\\{32\\}\\.md\\'" . joplin-mode))

;; Handle potential issue with backup files
;; Issue: https://discourse.joplinapp.org/t/note-for-emacs-users/623
;; Set backup file to a specific directory
(setq backup-directory-alist '(("." . "~/eSync/backups")))

;; =============================================================================
;; YAML Mode
;; =============================================================================

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'misc)
;;; misc.el ends here
