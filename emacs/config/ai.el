;;; ai.el --- AI integration -*- lexical-binding: t -*-

;;; Commentary:
;; This module provide Claude-ide integration

;; This module provides AI-powered code completion using GitHub Copilot
;; Uses official GitHub Copilot with existing license
;;
;; INSTALLATION (One-time setup):
;;   1. Install Node.js (required by Copilot):
;;      brew install node  # macOS
;;   2. Run: M-x copilot-install-server
;;   3. Run: M-x copilot-login (authenticate with GitHub)
;;
;; USAGE:
;;   - Ghost text appears as you type
;;   - Tab to accept suggestion (in insert mode)

;;   - M-n / M-p to cycle through alternatives
;;   - M-[ / M-] to accept word-by-word
;;   - C-g to dismiss

;;; Code:

;; =============================================================================
;; GitHub Copilot Integration
;; =============================================================================

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
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
  (set-face-attribute 'copilot-overlay-face nil :foreground "#6c7086" :italic t))

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
;; Minuet AI Completion 
;; =============================================================================
;; (use-package minuet
;;   :ensure t
;;   :vc (:url "https://github.com/milanglacier/minuet-ai.el"
;;             :rev :newest
;;             :branch "main")
;;   :bind
;;   (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
;;    ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
;;    ("C-c m" . #'minuet-configure-provider)
;;    :map minuet-active-mode-map
;;    ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
;;    ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
;;    ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
;;    ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
;;    ;; Accept the first line of completion, or N lines with a numeric-prefix:
;;    ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
;;    ("M-a" . #'minuet-accept-suggestion-line)
;;    ("M-e" . #'minuet-dismiss-suggestion))
;;   :hook (prog-mode . minuet-auto-suggestion-mode)
;;   :config
;;   (setq minuet-provider 'openai-fim-compatible)
;;   (setq minuet-n-completions 1) ; recommended for Local LLM for resource saving
;;   ;; I recommend beginning with a small context window size and incrementally
;;   ;; expanding it, depending on your local computing power. A context window
;;   ;; of 512, serves as an good starting point to estimate your computing
;;   ;; power. Once you have a reliable estimate of your local computing power,
;;   ;; you should adjust the context window to a larger value.
;;   (setq minuet-context-window 512)
;;   (plist-put minuet-openai-fim-compatible-options :end-point "http://100.79.1.45:11434/v1/completions")
;;   ;; an arbitrary non-null environment variable as placeholder.
;;   ;; For Windows users, TERM may not be present in environment variables.
;;   ;; Consider using APPDATA instead.
;;   (plist-put minuet-openai-fim-compatible-options :name "Ollama")
;;   (plist-put minuet-openai-fim-compatible-options :api-key "TERM")
;;   (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")

;;   (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56))

;; =============================================================================
;; Claude Code IDE
;; =============================================================================

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c c" . claude-code-ide-menu)
  :config
  ;; Compares with eat, vterm has better compatibility with evil.
  ;; However, the terminal output blocks cursor location,
  ;; use C-c C-t to enter terminal copy mode as workaround
  (setq claude-code-ide-terminal-backend 'vterm)
  ;; Configure diff view behavior to prevent "session reload" appearance
  (setq claude-code-ide-show-claude-window-in-ediff t)  ; Keep Claude visible during diff
  (claude-code-ide-emacs-tools-setup)
  ) ; Optionally enable Emacs MCP tools

(provide 'ai)
;;; ai.el ends here
