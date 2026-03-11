;;; org-visual.el --- Org mode visual configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org mode visual enhancements:
;; - Visual line and variable pitch modes
;; - Custom faces for headlines and elements
;; - Visual fill column for centered text
;; - Org superstar for headline bullets
;; - Emphasis markup helpers

;;; Code:

;;; --- Mode Setup ---

(defun my-org-mode-setup ()
  "Setup visual modes and local settings for Org mode."
  (visual-line-mode)
  (variable-pitch-mode 1)
  (setq-local line-spacing 0.2)
  (setq-local org-blank-before-new-entry
              '((heading . auto)
                (plain-list-item . auto))))

;;; --- Face Customizations ---

(defun my/org-visual-apply-faces ()
  "Apply Org face customizations without using `custom-set-faces'."
  (require 'org-indent)
  ;; Headings
  (set-face-attribute 'org-level-1 nil :inherit 'outline-1 :height 1.15)
  (set-face-attribute 'org-level-2 nil :inherit 'outline-2 :height 1.12)
  (set-face-attribute 'org-level-3 nil :inherit 'outline-3 :height 1.09)
  (set-face-attribute 'org-level-4 nil :inherit 'outline-4 :height 1.06)
  ;; Fixed-pitch elements (critical for alignment in variable-pitch mode)
  (set-face-attribute 'org-block            nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table            nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula          nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox         nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-indent           nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-special-keyword  nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line        nil :inherit '(font-lock-comment-face fixed-pitch)))

;;; --- Core Org Settings ---

(with-eval-after-load 'org
  (my/org-visual-apply-faces)
  (setq org-startup-indented t
        org-image-actual-width nil))

;;; --- Packages ---

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

;; Centered text layout
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; Headline bullets
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
  (set-face-attribute 'org-superstar-item nil :height 1.0))

;;; --- Hooks ---

(add-hook 'org-mode-hook #'my-org-mode-setup)
(add-hook 'org-mode-hook #'company-mode)

;;; --- Emphasis Markup Helpers ---

(defun my/wrap-with (char)
  "Wrap the visual selection with CHAR on both sides."
  (interactive "sWrap with: ")
  (let ((selection (buffer-substring (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat char selection char))))

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
