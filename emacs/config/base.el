;;; base.el --- Package setup and basic UI configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains:
;; - Package manager setup (package.el, use-package)
;; - Basic UI configuration (disable scrollbar, toolbar, etc.)
;; - Line numbers and visual settings

;;; Code:

;; =============================================================================
;; Package Setup
;; =============================================================================

    (require 'package)
    (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                             ("melpa-stable" . "https://stable.melpa.org/packages/")
                             ("org" . "https://orgmode.org/elpa/")
                             ("elpa" . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

  ;; ensures that the Emacs package archive is up-to-date before installing any new packages, by refreshing the package list if necessary.
    (unless package-archive-contents (package-refresh-contents))

    (require 'use-package)
    (setq use-package-always-ensure t)

;; =============================================================================
;; Basic UI Configuration
;; =============================================================================

  ;; Enable visible bell
  (setq visible-bell t)
  (setq ring-bell-function 'ignore)

  (setq inhibit-startup-message t)
  (scroll-bar-mode -1) ; Disable visible scrollbar
  (tool-bar-mode -1)   ; Disable the toolbar
  (tooltip-mode -1)    ; Disable tooltips
  (set-fringe-mode 10) ; Give some breathing room
  (menu-bar-mode -1)  ; Disable the menu bar


  (setq resize-mini-windows t) ; Grow and shrink the mini window as needed
  (setq max-mini-window-height 0.35) ; Cap 35% of frame height

  (setq split-width-threshold 160) ; the minimum width of a window that Emacs should split horizontally instead of vertically.
  (setq split-height-threshold 80)

  (column-number-mode)
  ;; Enable line numbers for some modes
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 1))))

  ;; Override some modes which derive from the above
  (dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
  ; Wrap text
  (global-visual-line-mode t)

(provide 'base)
;;; base.el ends here
