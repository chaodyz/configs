;;; base.el --- Package setup and basic UI configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains:
;; - Package manager setup (package.el, use-package)
;; - Basic UI configuration (disable scrollbar, toolbar, etc.)
;; - Line numbers and visual settings
;; - Built-in quality-of-life defaults

;;; Code:

;; =============================================================================
;; Package Setup
;; =============================================================================

(require 'package)

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"    . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; =============================================================================
;; Basic UI Configuration
;; =============================================================================

;; Enable visible bell
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(setq resize-mini-windows t)

(column-number-mode)

(global-auto-revert-mode 1)
;;  M-x cycle through commands
;;  M-p: previous-history-element
;;  M-x: next-history-element
(setq history-length 100)
(savehist-mode 1) 

(recentf-mode 1)

(winner-mode 1) ;; undo and redo window layout

(setq enable-recursive-minibuffers t)
(setq max-mini-window-height 0.39) ; Cap 39% of frame height

(save-place-mode 1) ;; remember the last file cursor position


(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(setq use-short-answers t)

;; Soft-wrap long lines at window edge (useful in split windows)
(global-visual-line-mode 1)

;; Scroll UX
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq scroll-margin 5
      scroll-conservatively 101)

;; Global auto-revert for all file buffers
(global-auto-revert-mode 1)

;; Optional: also revert non-file buffers (like Magit/Dired)
(setq global-auto-revert-non-file-buffers t)

;; Optional: silence the revert messages
(setq auto-revert-verbose nil)

(provide 'base)
;;; base.el ends here
