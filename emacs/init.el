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
   '("21d2bf8d4d1df4859ff94422b5e41f6f2eeff14dd12f01428fa3cb4cb50ea0fb"
     "82f4291a091046ecc4c6611ebc7c18f2a4aba9906db8514a6a2d1c8a84d78855"
     "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659"
     "c9d837f562685309358d8dc7fccb371ed507c0ae19cf3c9ae67875db0c038632"
     "e1df746a4fa8ab920aafb96c39cd0ab0f1bac558eff34532f453bd32c687b9d6"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "599f72b66933ea8ba6fce3ae9e5e0b4e00311c2cbf01a6f46ac789227803dd96"
     "c3c135e69890de6a85ebf791017d458d3deb3954f81dcb7ac8c430e1620bb0f1"
     "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "2f8af2a3a2fae6b6ea254e7aab6f3a8b5c936428b67869cef647c5f8e7985877"
     "2ab8cb6d21d3aa5b821fa638c118892049796d693d1e6cd88cb0d3d7c3ed07fc"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "b99ff6bfa13f0273ff8d0d0fd17cc44fab71dfdc293c7a8528280e690f084ef0"
     "0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     "2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
     "0f9a1b7a0f1d09544668297c1f04e5a5452ae1f4cf69f11b125f4cff1d54783d"
     "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "671c79fc459f28077436448cef3b597064676ca2dc6b00f29f522a6137dd2c22"
     "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "9f1c593abc996917c24f563e68f44bb4175d4419925577014757f6ba2dfe2850"
     "7faf118c5f84a233f63dfafddfe04cd1cfb011728589192f29ced2bdc465b527"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "42a6583a45e0f413e3197907aa5acca3293ef33b4d3b388f54fa44435a494739"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "5a4cdc4365122d1a17a7ad93b6e3370ffe95db87ed17a38a94713f6ffe0d8ceb"
     "972f792651d32b0506481b9e87b2fbc9b732ae9da2527562668c6e7d149fefda"
     default))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code-ide :url
                      "https://github.com/manzaltu/claude-code-ide.el")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")))
 '(safe-local-variable-values '((evil-shift-width . 2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
