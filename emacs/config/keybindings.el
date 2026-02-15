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


  ;; Switch buffer in current project
  (global-set-key (kbd "C-S-b") 'counsel-projectile-switch-to-buffer)

  ;; Switch between projects
  (global-set-key (kbd "C-S-p") 'counsel-projectile-switch-project)

  ;; Quick buffer switch (all buffers, not just project)
  (global-set-key (kbd "C-b") 'counsel-ibuffer)

;; =============================================================================
;; General (Leader Key Configuration)
;; =============================================================================

  ;; Smart quit: close buffer if only window, otherwise close window
  (defun my/smart-quit ()
    "Close window if multiple windows exist, otherwise kill buffer."
    (interactive)
    (if (one-window-p)
        (kill-buffer)
      (delete-window)))

  (use-package general
    :ensure t
    :after evil
    :requires which-key
    :config
    (general-override-mode)
    (general-create-definer leader-key-def
      :keymaps 'override
      :states '(normal visual motion emacs)
      :prefix "SPC"
      :non-normal-prefix "M-SPC")
    (leader-key-def
      "d" 'describe-thing-at-point
      "f" 'counsel-find-file
      "h" 'counsel-command-history
      "p" 'projectile-command-map
      "q" 'my/smart-quit :which-key "Close buffer/window"
      "r" 'counsel-recentf
      "s" 'counsel-projectile-rg :which-key " Search in project"
      "w" 'save-buffer
      "R" 'restart-emacs
      "v" 'vterm :which-key " Vterm"
      )
    (leader-key-def
      "b" '(:ignore t :which-key " Buffer...")
      "b l" 'counsel-ibuffer
      "b c" 'kill-buffer
      "b w" 'save-buffer 
      )
    
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
      "c c" 'claude-code-ide :which-key "Open Console"
      "c r" 'claude-code-ide-resume :which-key "Resume session"
      "c l" 'claude-code-ide-list-sessions :which-key "List sessions"
      "c x" 'claude-code-ide-stop :which-key "Kill"
     )

    (leader-key-def
      "g" '(:ignore t :which-key " Magit...")
      "g s" 'magit-status
      "g b" 'magit-blame
      "g l" 'magit-log-buffer-file
      "g g" 'magit-dispatch
      "g c" 'magit-commit-create)

    (leader-key-def
      "l" '(:ignore t :which-key " LSP/Code...")
      "l d" 'xref-find-definitions :which-key "Go to definition"
      "l r" 'xref-find-references :which-key "Find references"
      "l n" 'eglot-rename :which-key "Rename symbol"
      "l a" 'eglot-code-actions :which-key "Code actions"
      "l f" 'eglot-format :which-key "Format"
      "l p" 'apheleia-format-buffer :which-key "Format with Prettier"
      "l h" 'eldoc-doc-buffer :which-key "Show documentation"
      "l e" 'flymake-show-buffer-diagnostics :which-key "Show errors")
    )

(provide 'keybindings)
;;; keybindings.el ends here
