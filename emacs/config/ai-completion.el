;;; ai-completion.el --- AI-powered code completion -*- lexical-binding: t -*-

;;; Commentary:
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

(provide 'ai-completion)
;;; ai-completion.el ends here
