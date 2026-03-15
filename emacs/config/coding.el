;;; coding.el --- Development tools configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains development tools:
;; - Treesitter for syntax highlighting
;; - Eglot for LSP support
;; - Counsel-projectile integration
;; - Magit for git integration
;; - Markdown mode
;;; Code:

;; =============================================================================
;; Magit (Git Interface)
;; =============================================================================

(use-package magit
  :ensure t)

;; =============================================================================
;; Ediff Configuration
;; =============================================================================

;; Split windows side-by-side (left/right) instead of top/bottom
(setq ediff-split-window-function 'split-window-horizontally)

;; Put Ediff control panel in the same frame (not a separate window)
;; This works better with Evil mode and window management
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Restore window config after quitting ediff
(add-hook 'ediff-quit-hook 'winner-undo)

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
          html-ts-mode
          mhtml-mode
          js-ts-mode
          python-mode
          python-ts-mode
          java-mode
          c-mode
          c++-mode
          js-mode
          typescript-mode
          go-mode
          go-ts-mode
          rust-mode
          json-mode
          json-ts-mode
          sh-mode
          bash-ts-mode) . eglot-ensure)
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

  ;; Configure bash language server for shell scripts
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))

  ;; Angular Language Server for Angular projects, TypeScript LS for everything else.
  ;; Uses Angular LS v14 (node v14) which matches Angular 14 / TypeScript 4.8 projects.
  ;; Both TS and HTML files use the same server so template navigation has full TS context.
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode typescript-mode html-ts-mode mhtml-mode)
                 . (lambda (&optional _interactive)
                     (if-let ((project-root (locate-dominating-file default-directory "angular.json")))
                         (let* ((node-modules (expand-file-name "node_modules" project-root))
                                (node-bin "/home/diz/.local/share/fnm/node-versions/v14.21.3/installation/bin/node")
                                (ngserver "/home/diz/.local/share/fnm/node-versions/v14.21.3/installation/lib/node_modules/@angular/language-server/bin/ngserver"))
                           (list node-bin ngserver
                                 "--stdio"
                                 "--tsProbeLocations" node-modules
                                 "--ngProbeLocations" node-modules))
                       '("typescript-language-server" "--stdio")))))

  ;; Show documentation on hover (like VS Code)
  (setq eldoc-echo-area-use-multiline-p nil))  ; Keep it concise

;; Format on save (optional, uncomment if you want this)
;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (eglot-managed-p)
;;               (eglot-format-buffer))))

;; gd replaces current window instead of splitting (default is 'other-window)
(setq xref-window-type 'same-window)

;; Note: Ensure language servers are installed:
;;   npm install -g @angular/language-service@latest
;;   npm install -g typescript
;;   npm install -g vscode-langservers-extracted  # Includes JSON, HTML, CSS, ESLint servers
;;   npm install -g bash-language-server

;; =============================================================================
;; Apheleia
;; =============================================================================
(use-package apheleia
  :ensure t
  :init
  (setq apheleia-log-debug-info t)
  :config
  ;; Use explicit formatter mappings for the modes used in this config so
  ;; format-on-save is predictable across JS/TS, markup, shell, and Python.
  (setf (alist-get 'javascript-mode apheleia-mode-alist) 'prettier
        (alist-get 'js-mode apheleia-mode-alist) 'prettier
        (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'typescript-mode apheleia-mode-alist) 'prettier
        (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'json-mode apheleia-mode-alist) 'prettier
        (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'css-mode apheleia-mode-alist) 'prettier
        (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'html-mode apheleia-mode-alist) 'prettier
        (alist-get 'html-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'mhtml-mode apheleia-mode-alist) 'prettier
        (alist-get 'markdown-mode apheleia-mode-alist) 'prettier
        (alist-get 'yaml-mode apheleia-mode-alist) 'prettier
        (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'sh-mode apheleia-mode-alist) 'shfmt
        (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt
        (alist-get 'python-mode apheleia-mode-alist) 'black
        (alist-get 'python-ts-mode apheleia-mode-alist) 'black)

  (apheleia-global-mode +1))


(provide 'coding)
;;; coding.el ends here
