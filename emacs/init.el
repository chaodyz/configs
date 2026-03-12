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
 '(custom-safe-themes
   '("972f792651d32b0506481b9e87b2fbc9b732ae9da2527562668c6e7d149fefda"
     "5a4cdc4365122d1a17a7ad93b6e3370ffe95db87ed17a38a94713f6ffe0d8ceb"
     "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "42a6583a45e0f413e3197907aa5acca3293ef33b4d3b388f54fa44435a494739"
     "87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     default))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code-ide :url
                      "https://github.com/manzaltu/claude-code-ide.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
