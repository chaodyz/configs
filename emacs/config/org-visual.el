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
    ;;smartly decide when to insert a blank line based on context.
    (setq-local org-blank-before-new-entry
                '((heading . auto)
                  (plain-list-item . auto)))
     ;; (my/org-fixed-pitch-faces)
    )
  ;; Set faces for headings, lists, and other elements
  (custom-set-faces
   ;; Set font and size for headlines
   '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.12))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.09))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.06))))
   '(org-default ((t (:inherit default :height 1.0))))
   '(org-block ((t (:inherit fixed-pitch :height 0.9))))
   '(org-code ((t (:inherit (shadow fixed-pitch) :height 0.9))))
   '(org-link ((t (:inherit link :height 1.0))))
   '(org-ellipsis ((t (:inherit default :weight normal :height 1.0 :underline nil)))))

  ;; This enables company-mode (autocomplete) when you’re editing Org files.
  (add-hook 'org-mode-hook #'company-mode)
  ;;This tells Org to display inline images at their natural size (actual pixel width), rather than scaling them down.
  (setq org-image-actual-width nil)



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
          org-superstar-prettify-item-bullets t
          org-superstar-headline-bullets-list '("☵" "○" "✻" "✿" "◆" "▶" "◉" "⚛" "♠" "☯" "✦" "⚝" "♢" "✸" "⬢"))
    :config
    ;; consistent size for list bullets
    (set-face-attribute 'org-superstar-item nil :height 1.0))

;; Hook to enable visual settings
(add-hook 'org-mode-hook 'my-org-mode-setup)

;; =============================================================================
;; Helper Functions
;; =============================================================================

  (defun my/org-fixed-pitch-faces () (require 'org-indent) (dolist (face '(org-block org-table org-formula org-code org-indent org-verbatim org-special-keyword org-meta-line org-checkbox)) (set-face-attribute face nil :inherit 'fixed-pitch)) (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch)) (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch)) (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch)) (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch)) (set-face-attribute 'org-column nil :background nil) (set-face-attribute 'org-column-title nil :background nil))

    ;;  Helper emphasis
      (defun my-wrap-with-stars ()
        "Wrap visual selection with *."
        (interactive)
        (let ((selection (buffer-substring-no-properties
                          (region-beginning) (region-end))))
          (delete-region (region-beginning) (region-end))
          (insert (concat "*" selection "*"))))

      (defun my-wrap-with-tides ()
        "Wrap visual selection with ~."
        (interactive)
        (let ((selection (buffer-substring-no-properties
                          (region-beginning) (region-end))))
          (delete-region (region-beginning) (region-end))
          (insert (concat "~" selection "~"))))

      (defun my-wrap-with-equals ()
        "Wrap visual selection with =."
        (interactive)
        (let ((selection (buffer-substring-no-properties
                          (region-beginning) (region-end))))
          (delete-region (region-beginning) (region-end))
          (insert (concat "=" selection "="))))

      ;; Bind the function to a key combination
      (define-key evil-visual-state-map (kbd "C-*") 'my-wrap-with-stars)
      (define-key evil-visual-state-map (kbd "C-~") 'my-wrap-with-tides)
      (define-key evil-visual-state-map (kbd "C-=") 'my-wrap-with-equals)

      ;; Surround with ANY KEY (chatGPT)
      (defun surround-with-key (beg end key)
        "Surround the region between BEG and END with KEY."
        (interactive "r\nsSurround with: ")
        (goto-char end)
        (insert key)
        (goto-char beg)
        (insert key))

      (general-define-key
       :states '(visual)
       :keymaps 'override
       "s" 'surround-with-key)

(provide 'org-visual)
;;; org-visual.el ends here
