;;; completion.el --- Completion frameworks configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains completion and search frameworks:
;; - Ivy: Generic completion frontend
;; - Ivy-rich: Enhanced Ivy display
;; - Counsel: Ivy-enhanced commands
;; - Company: In-buffer completion

;;; Code:

;; =============================================================================
;; Ivy, Ivy-rich, and Counsel
;; =============================================================================

;; Note: ivy-rich must be setup after Ivy and counsel

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-recentf))
  :config
  (setq ivy-initial-inputs-alist nil))

;; =============================================================================
;; Company (In-buffer Completion)
;; =============================================================================

(use-package company
  :ensure t
  :config
  (global-company-mode 1))

(provide 'completion)
;;; completion.el ends here
