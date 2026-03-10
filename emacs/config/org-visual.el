;;; org-visual.el --- Org mode visual configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains Org mode visual enhancements:
;; - Visual line and variable pitch modes
;; - Custom faces for headlines and elements
;; - Company mode integration
;; - Visual fill column for centered text
;; - Org superstar for headline bullets
;; - Helper functions for emphasis markup

;;; Code:

;; Configure org mode to start with modes that more visual appealing
;; - visual-line-mode: wraps lines at window width for easy reading and editing
;; - variable-pitch-mode 1: sets the font face to a variable-width font for a more natural and aesthetically pleasing look
(defun my-org-mode-setup ()
  "Setup visual line and variable pitch modes for Org mode."
  (visual-line-mode)
  (variable-pitch-mode 1)
  (setq-local line-spacing 0.2)
  (setq-local org-blank-before-new-entry
              '((heading . auto)
                (plain-list-item . auto))))

(defun my/org-visual-apply-faces ()
  "Apply Org face customizations without using `custom-set-faces'."
  (require 'org-indent)
  ;; Headings
  (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.15)
  (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.12)
  (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.09)
  (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.06)
  (set-face-attribute 'org-default nil :inherit 'default :height 1.0)
  (set-face-attribute 'org-link nil :inherit 'link :height 1.0)
  (set-face-attribute 'org-ellipsis nil :inherit 'default :weight 'normal :height 1.0 :underline nil)
  ;; Fixed-pitch elements
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch :height 0.9)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch) :height 0.9)
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

(with-eval-after-load 'org
  (my/org-visual-apply-faces))

;; This enables company-mode (autocomplete) when you’re editing Org files.
(add-hook 'org-mode-hook #'company-mode)
;;This tells Org to display inline images at their natural size (actual pixel width), rather than scaling them down.
(setq org-image-actual-width nil)

;; Paste/download images into Org buffers
(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/eSync/org/pictures")
  (org-download-heading-lvl nil)
  (org-download-display-inline-images t)
  :config
  (setq-default org-download-image-org-width 800)
  (setq-default org-download-image-html-width 800)
  (add-hook 'dired-mode-hook 'org-download-enable))



;; feat(visual): center text
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
      	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))


(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; headline bullet
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-remove-leading-stars t
  	    org-superstar-leading-bullet " "
        org-hide-leading-stars t
        org-indent-mode-turns-on-hiding-stars t
        org-superstar-prettify-item-bullets t)
  :config
  ;; consistent size for list bullets
  (set-face-attribute 'org-superstar-item nil :height 1.0))

;; Hook to enable visual settings
(add-hook 'org-mode-hook 'my-org-mode-setup)

;; Declare a wrap with function that wraps selection with CHAR pairs
(defun my/wrap-with (char)
  "Wrap the visual selection with CHAR on both sides."
  (interactive "sWrap with: ")
  (let ((selection (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat char selection char))))

;; Bind the function to a key combination
(define-key evil-visual-state-map (kbd "C-*") (lambda () (interactive) (my/wrap-with "*")))
(define-key evil-visual-state-map (kbd "C-~") (lambda () (interactive) (my/wrap-with "~")))
(define-key evil-visual-state-map (kbd "C-=") (lambda () (interactive) (my/wrap-with "=")))
(define-key evil-visual-state-map (kbd "C-/") (lambda () (interactive) (my/wrap-with "/")))
(define-key evil-visual-state-map (kbd "C-+") (lambda () (interactive) (my/wrap-with "+")))

(general-define-key
 :states '(visual)
 :keymaps 'override
 "s" 'surround-with-key)

(provide 'org-visual)
;;; org-visual.el ends here
