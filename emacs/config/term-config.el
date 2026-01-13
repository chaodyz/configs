;;; shell.el --- Shell and terminal configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains shell and terminal configuration:
;; - Built-in term mode with 256 color support
;; - VTerm for better terminal emulation

;;; Code:

;; =============================================================================
;; Built-in Term Mode
;; =============================================================================

(use-package term
  :ensure nil  ; Built-in package
  :config
  (setq explicit-shell-file-name "bash"))

(use-package eterm-256color
  :ensure t
  :hook (term-mode . eterm-256color-mode))

;; =============================================================================
;; VTerm (Enhanced Terminal Emulator)
;; =============================================================================

(use-package vterm
  :ensure t
  :commands vterm
  :config
  ;; Set this to match your custom shell prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  ;; Set this to customize the shell to launch
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 10000))

(provide 'term-config)
;;; shell.el ends here
