;;; init.el --- Minimal Bootstrap Configuration -*- lexical-binding: t -*-

;; Author: diz
;; Description: Modular Emacs configuration - loads all modules from config/

;;; Commentary:
;; This is a minimal bootstrap file that loads all configuration modules
;; from the config/ directory. Each module handles a specific aspect of
;; the configuration.

;;; Code:

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; Load modules in order
;; Core setup - must be loaded first
(require 'base)           ; Package setup and basic UI

;; Visual configuration
(require 'fonts)          ; Font configuration
(require 'theme)          ; Theme and circadian

;; Editing experience
(require 'editing)        ; Evil mode, undo-tree
(require 'completion)     ; Ivy, counsel, company
(require 'keybindings)    ; General, which-key, hydra

;; Org mode - loaded in dependency order
(require 'org-core)       ; Basic org settings & babel
(require 'org-visual)     ; Visual enhancements
(require 'org-workflow)   ; Agenda, todo, capture
(require 'org-roam)       ; Roam configuration

;; Development tools
(require 'development)    ; LSP, treesitter, projectile, magit

;; Language-specific
(require 'chinese)        ; Pyim (拼音)

;; Other tools
(require 'term-config)    ; Terminal configuration
(require 'misc)           ; Helpful, restart-emacs, etc.

;;; init.el ends here
