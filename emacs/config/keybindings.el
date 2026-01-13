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

  ;; Which key
  (use-package which-key
    :ensure t
    :diminish
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.3)
    (setq which-key-prefix-prefix "SPC")
    (setq which-key-allow-evil-operators t)
    )

;; =============================================================================
;; General (Leader Key Configuration)
;; =============================================================================

  (use-package general
    :ensure t
    :requires which-key
    :config
    (general-create-definer leader-key-def
      :states '(normal visual insert emacs)
      :prefix "SPC"
      :non-normal-prefix "M-SPC")
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
      "v" 'vterm :which-key " Vterm"
      )
    (leader-key-def
      "b" '(:ignore t :which-key " Buffer...")
      "b l" 'counsel-ibuffer
      "b c" 'kill-buffer
      "b w" 'save-buffer 
      )
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
      "o it" '(org-toggle-inline-images :which-key "Toggle inline image")
      )
    (leader-key-def
      "c" '(:ignore t :which-key " Claude Console...")
      "c t" 'claude-code-ide)
    (leader-key-def
      "g" '(:ignore t :which-key " Magit...")
      "g s" 'magit-status
      "g b" 'magit-blame
      "g l" 'magit-log-buffer-file
      "g g" 'magit-dispatch
      "g c" 'magit-commit-create)
    )

(provide 'keybindings)
;;; keybindings.el ends here
