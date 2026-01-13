;;; keybindings.el --- Keybinding configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains:
;; - Which-key: Display available keybindings
;; - General: Leader key and keybinding definitions
;; - All SPC-prefixed keybindings for various modes

;;; Code:

;; =============================================================================
;; Which Key
;; =============================================================================

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-prefix-prefix "SPC")
  (setq which-key-allow-evil-operators t))

;; =============================================================================
;; General (Leader Key Configuration)
;; =============================================================================

(use-package general
  :ensure t
  :requires which-key
  :config

  ;; Define leader key
  (general-create-definer leader-key-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  ;; First level keybindings
  (leader-key-def
    "d" 'describe-thing-at-point
    "f" 'counsel-find-file
    "h" 'counsel-command-history
    "p" 'projectile-command-map
    "q" 'delete-window
    "r" 'counsel-recentf
    "w" 'save-buffer
    "R" 'restart-emacs
    "e" 'neotree-toggle :which-key " Neotree"
    "v" 'vterm :which-key " Vterm")

  ;; Claude code IDE comands
  (leader-key-def
    "c" '(:ignore t :which-key " Claude Console...")
    "c t" 'claude-code-ide)
  ;; Buffer commands
  (leader-key-def
    "b" '(:ignore t :which-key " Buffer...")
    "b l" 'counsel-ibuffer
    "b c" 'kill-buffer
    "b w" 'save-buffer)

  ;; Window commands
  (leader-key-def
    "a" '(:ignore t :which-key " Window...")
    "a v" #'split-window-right
    "a s" #'split-window-below
    "a w" #'other-window
    "a o" #'delete-other-windows
    "a h" #'evil-window-left
    "a j" #'evil-window-down
    "a k" #'evil-window-up
    "a l" #'evil-window-right
    "a H" #'evil-window-move-far-left
    "a J" #'evil-window-move-very-bottom
    "a K" #'evil-window-move-very-top
    "a L" #'evil-window-move-far-right)

  ;; Magit commands
  (leader-key-def
    "g" '(:ignore t :which-key " Magit...")
    "g s" 'magit-status
    "g b" 'magit-blame
    "g l" 'magit-log-buffer-file
    "g g" 'magit-dispatch
    "g c" 'magit-commit-create)

  ;; LSP/Eglot commands
  (leader-key-def
    "l"   '(:ignore t :which-key "LSP/Eglot")
    "l r" #'xref-find-references
    "l d" #'xref-find-definitions
    "l i" #'eglot-find-implementation
    "l D" #'eglot-find-typeDefinition
    "l a" #'eglot-code-actions
    "l R" #'eglot-rename
    "l f" #'eglot-format
    "l e" #'flymake-show-buffer-diagnostics)

  ;; Org commands
  (leader-key-def
    "o" '(:ignore t :which-key " Org...")
    "o a" 'org-agenda
    "o b" '(org-babel-tangle :which-key "Org Babel Tangle")
    "o c" 'org-capture
    "o d" 'org-deadline
    "o s" 'org-schedule
    "o o" 'org-open-at-point
    "o t" '(counsel-org-tag :which-key "Set Org Tag")
    "o r" '(:ignore t :which-key " Org Roam")
    "o rf" '(org-roam-node-find :which-key "Find a Node")
    "o ri" '(org-roam-node-insert :which-key "Insert a Node")
    "o rr" '(org-roam-buffer-toggle :which-key "Toggle Org Roam Buffer")
    "o it" '(org-toggle-inline-images :which-key "Toggle inline image")))

(provide 'keybindings)
;;; keybindings.el ends here
