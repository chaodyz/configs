;;; ai.el --- AI integration -*- lexical-binding: t -*-

;;; Commentary:
;; This module provide Claude-ide integration

;; This module provides AI-powered code completion using GitHub Copilot
;; Uses official GitHub Copilot with existing license
;; Also includes EditorConfig support for consistent formatting
;;
;; INSTALLATION (One-time setup):
;;
;;   1. Clone the repository:
;;      git clone https://github.com/copilot-emacs/copilot.el ~/.emacs.d/copilot.el
;;
;;   2. Install Node.js (required by Copilot):
;;      brew install node  # macOS
;;      # or use your system's package manager
;;
;;   3. Restart Emacs
;;
;;   4. Run: M-x copilot-install-server (one-time setup)
;;
;;   5. Run: M-x copilot-login (authenticate with GitHub)
;;
;; USAGE:
;;   - Ghost text appears as you type
;;   - Tab to accept suggestion (in insert mode)
;;   - M-n / M-p to cycle through alternatives
;;   - M-[ / M-] to accept word-by-word
;;   - C-g to dismiss

;;; Code:

;; =============================================================================
;; EditorConfig Support
;; =============================================================================

;; Automatically apply .editorconfig settings (indent_size, indent_style, etc.)
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Minimal fallback for files without .editorconfig
(setq-default indent-tabs-mode nil)      ; Use spaces by default
(setq-default tab-width 2)               ; Display tabs as 2 spaces

;; =============================================================================
;; GitHub Copilot Integration
;; =============================================================================

;; Add copilot to load path (requires manual clone)
(add-to-list 'load-path "~/.emacs.d/copilot.el")

;; Load copilot if available
(when (file-directory-p "~/.emacs.d/copilot.el")
  (use-package copilot
    :ensure nil
    :hook (prog-mode . copilot-mode)
    :bind (:map copilot-completion-map
                ;; Tab to accept (works in insert mode)
                ("<tab>" . copilot-accept-completion)
                ("TAB" . copilot-accept-completion)

                ;; Word-by-word acceptance
                ("M-<right>" . copilot-accept-completion-by-word)
                ("M-]" . copilot-accept-completion-by-word)

                ;; Line-by-line acceptance
                ("M-<down>" . copilot-accept-completion-by-line)

                ;; Cycle through suggestions
                ("M-n" . copilot-next-completion)
                ("M-p" . copilot-previous-completion)

                ;; Dismiss
                ("C-g" . copilot-clear-overlay))

    :config
    ;; Show copilot status in mode line
    (setq copilot-mode-line-format " Copilot")

    ;; Suppress indentation warning (harmless)
    (setq copilot-indent-offset-warning-disable t)

    ;; Set default indentation to match .editorconfig (2 spaces)
    (setq copilot-indent-offset-alist
          '((c-mode . 2)
            (c++-mode . 2)
            (java-mode . 2)
            (js-mode . 2)
            (js-ts-mode . 2)
            (typescript-mode . 2)
            (typescript-ts-mode . 2)
            (tsx-ts-mode . 2)
            (python-mode . 2)
            (go-mode . 2)
            (rust-mode . 2)
            (json-mode . 2)
            (json-ts-mode . 2)
            (css-mode . 2)
            (sh-mode . 2)
            (markdown-mode . 2)))

    ;; Customize ghost text appearance (optional)
    (custom-set-faces
     '(copilot-overlay-face ((t (:foreground "#6c7086" :italic t)))))))

;; =============================================================================
;; Integration with Company
;; =============================================================================

;; Make sure Tab works for Copilot, not Company
(with-eval-after-load 'company
  ;; Disable Tab in company (let Copilot handle it)
  (define-key company-active-map (kbd "<tab>") nil)
  (define-key company-active-map (kbd "TAB") nil)

  ;; Use C-n/C-p for company completion instead
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))


;; =============================================================================
;; Claude Code IDE
;; =============================================================================

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  ;; Configure diff view behavior to prevent "session reload" appearance
  (setq claude-code-ide-show-claude-window-in-ediff t)  ; Keep Claude visible during diff
  (setq claude-code-ide-focus-claude-after-ediff nil)   ; Don't switch focus to Claude after diff opens
  (setq claude-code-ide-switch-tab-on-ediff nil)        ; Don't switch tabs when opening diff
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools


(use-package ai-code
  :ensure t
  :config
  ;; use codex as backend, other options are 'claude-code, 'gemini, 'github-copilot-cli, 'opencode, 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'claude-code-ide, 'claude-code-el
  (ai-code-set-backend 'claude-code-ide)
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  ;; Optional: Use eat if you prefer, by default it is vterm
  ;; (setq ai-code-backends-infra-terminal-backend 'eat) ;; the way to config all native supported CLI. for external backend such as claude-code-ide.el and claude-code.el, please check their config
  ;; Optional: Enable @ file completion in comments and AI sessions
  (ai-code-prompt-filepath-completion-mode 1)
  ;; Optional: Ask AI to run test after code changes, for a tighter build-test loop
  (setq ai-code-auto-test-type 'test-after-change)
  ;; Optional: In AI session buffers, SPC in Evil normal state triggers the prompt-enter UI
  (with-eval-after-load 'evil (ai-code-backends-infra-evil-setup))
  ;; Optional: Turn on auto-revert buffer, so that the AI code change automatically appears in the buffer
  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1) ;; set to 1 second for faster update
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))


(provide 'ai)
;;; ai.el ends here
