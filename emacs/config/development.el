;;; development.el --- Development tools configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains development tools:
;; - Treesitter for syntax highlighting
;; - Eglot for LSP support
;; - Projectile for project management
;; - Counsel-projectile integration
;; - Magit for git integration

;;; Code:

;; =============================================================================
;; Tree-sitter (Syntax Highlighting)
;; =============================================================================

;; Tree-sitter-auto helps treesitter find the missing major mode
;; Useful for formats like tsx
;; Reference: https://github.com/renzmann/treesit-auto
;; Author's blog: https://robbmann.io/posts/emacs-treesit-auto/

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; =============================================================================
;; LSP (Language Server Protocol)
;; =============================================================================

;; Emacs 29+ includes eglot, a lightweight, built-in LSP client
;; No need to install external LSP clients unless you prefer alternatives like lsp-mode

(use-package eglot
  :ensure nil
  :hook ((tsx-ts-mode
          typescript-ts-mode
          js-ts-mode
          python-mode
          java-mode
          c-mode
          c++-mode
          js-mode
          typescript-mode
          go-mode
          rust-mode) . eglot-ensure)
  :config
  ;; Prefer xref for definitions
  (setq xref-show-definitions-function xref-show-xrefs-function))

;; Note: For TypeScript, ensure language servers are installed:
;;   npm install -g @angular/language-service@latest
;;   npm install -g typescript

;; =============================================================================
;; Projectile (Project Management)
;; =============================================================================

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/projects/"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

;; =============================================================================
;; Magit (Git Interface)
;; =============================================================================

(use-package magit
  :ensure t)

;; =============================================================================
;; Markdown Mode
;; =============================================================================

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-export)))

;;================================
;; Claude Code IDE 
;;=================================
(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'development)
;;; development.el ends here
