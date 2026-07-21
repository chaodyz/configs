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
(require 'ai)  ; AI Integrations
(require 'keybindings)    ; General, which-key, hydra

;; Org mode - loaded in dependency order
(require 'org-core)       ; Basic org settings & babel
(require 'org-visual)     ; Visual enhancements
(require 'org-workflow)   ; Agenda, todo, capture
;;(require 'org-roam)       ; Roam configuration

;; Development tools
(require 'coding)         ; LSP, treesitter, projectile, magit

;; Language-specific
(require 'chinese)        ; Pyim (拼音)

;; Other tools
(require 'term-config)    ; Terminal configuration
(require 'misc)           ; Helpful, restart-emacs, etc.

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((eldoc-box :url "https://github.com/casouri/eldoc-box" :branch
                "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
