;;; ai-completion.el --- AI-powered code completion -*- lexical-binding: t -*-

;;; Commentary:
;; This module provides AI-powered code completion using Codeium
;; Codeium is a free alternative to GitHub Copilot
;;
;; INSTALLATION (One-time setup):
;;
;;   1. Clone the repository:
;;      git clone --depth 1 https://github.com/Exafunction/codeium.el ~/.emacs.d/codeium.el
;;
;;   2. Restart Emacs
;;
;;   3. Run: M-x codeium-install (downloads the language server)
;;
;;   4. Run: M-x codeium-init (authenticate - opens browser)
;;
;; USAGE:
;;   - AI suggestions appear in completion menu
;;   - Works with Company mode
;;   - Tab to accept completion
;;
;; REQUIREMENTS:
;;   - Emacs compiled with libxml2 support
;;   - Check with: M-: (libxml-available-p)
;;; Code:

;; =============================================================================
;; Codeium (Free AI Code Completion)
;; =============================================================================

;; Add codeium to load path (requires manual clone - see instructions above)
(add-to-list 'load-path "~/.emacs.d/codeium.el")

;; Only load if codeium is available
(when (file-directory-p "~/.emacs.d/codeium.el")
  (use-package codeium
    :ensure nil  ; Don't try to install from package manager
    :init
    ;; Add codeium to completion-at-point-functions
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

    :config
    ;; Disable dialog boxes (use minibuffer instead)
    (setq use-dialog-box nil)

    ;; Show codeium status in mode line (optional)
    (setq codeium-mode-line-enable
          (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

    ;; Enable codeium in programming modes
    (add-hook 'prog-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)))))

;; =============================================================================
;; Company Mode Configuration for Codeium
;; =============================================================================

;; Optimize company for better AI completion experience
(with-eval-after-load 'company
  ;; Faster completion popup
  (setq company-idle-delay 0.1)
  ;; Start completing after 1 character
  (setq company-minimum-prefix-length 1)
  ;; Don't require exact match
  (setq company-require-match nil))

(provide 'ai-completion)
;;; ai-completion.el ends here
