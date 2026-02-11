;;; development.el --- Development tools configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains development tools:
;; - Treesitter for syntax highlighting
;; - Eglot for LSP support
;; - Projectile for project management
;; - Counsel-projectile integration
;; - Magit for git integration
;; - Markdown mode
;; - Claude Code IDE

;;; Code:

;; =============================================================================
;; Projectile (Project Management)
;; =============================================================================

  ;; Projectile
  (use-package projectile
    :ensure t
    :init
    (setq projectile-completion-system 'ivy)
    :config
    (projectile-mode 1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-project-search-path '("~/projects/" "~/projects/backup/"))
    (setq projectile-switch-project-action #'projectile-dired))

  (use-package counsel-projectile
    :ensure t
    :config (counsel-projectile-mode))

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
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.mdx\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-export)))

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

;; =============================================================================
;; Tree-sitter (Syntax Highlighting)
;; =============================================================================

;; Tree-sitter-auto helps treesitter find the missing major mode
;; Useful for formats like tsx
;; Reference: https://github.com/renzmann/treesit-auto
;; Author's blog: https://robbmann.io/posts/emacs-treesit-auto/

  ;; Configure tree-sitter language sources
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (use-package treesit-auto
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
          rust-mode
          json-mode
          json-ts-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ;; Go to definition (F12 like VS Code)
              ("<f12>" . xref-find-definitions)
              ("C-<f12>" . xref-find-definitions-other-window)
              ;; Find references (Shift+F12 like VS Code)
              ("S-<f12>" . xref-find-references)
              ;; Rename symbol (F2 like VS Code)
              ("<f2>" . eglot-rename)
              ;; Code actions (Ctrl+. like VS Code)
              ("C-." . eglot-code-actions)
              ;; Show documentation (hover)
              ("C-h ." . eldoc-doc-buffer)
              ;; Format buffer/region
              ("C-S-i" . eglot-format))
  :config
  ;; Configure TypeScript language server for MDX files
  ;; MDX files open in markdown-mode, manually run M-x eglot to activate
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("typescript-language-server" "--stdio")))

  ;; Configure JSON language server for schema validation
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode) . ("vscode-json-languageserver" "--stdio")))

  ;; Prefer xref for definitions
  (setq xref-show-definitions-function xref-show-xrefs-function)

  ;; Show documentation on hover (like VS Code)
  (setq eldoc-echo-area-use-multiline-p nil))  ; Keep it concise

  ;; Format on save (optional, uncomment if you want this)
  ;; (add-hook 'before-save-hook
  ;;           (lambda ()
  ;;             (when (eglot-managed-p)
  ;;               (eglot-format-buffer))))

  ;; Evil mode bindings for LSP (Vim-style shortcuts)
  (with-eval-after-load 'evil
    (evil-define-key 'normal eglot-mode-map
      (kbd "gd") 'xref-find-definitions           ; Go to definition
      (kbd "gD") 'xref-find-definitions-other-window
      (kbd "gr") 'xref-find-references            ; Go to references
      (kbd "K") 'eldoc-doc-buffer                 ; Show docs (like Vim K)
      (kbd "C-.") 'eglot-code-actions))

;; Note: Ensure language servers are installed:
;;   npm install -g @angular/language-service@latest
;;   npm install -g typescript
;;   npm install -g vscode-langservers-extracted  # Includes JSON, HTML, CSS, ESLint servers

(provide 'development)
;;; development.el ends here
