;;; coding.el --- Development tools configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains development tools:
;; - Project.el for project management
;; - Treesitter for syntax highlighting
;; - Eglot for LSP support
;; - Magit for git integration
;; - Markdown mode
;; - Apheleia for format-on-save
;;; Code:

;; =============================================================================
;; Magit (Git Interface)
;; =============================================================================

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; =============================================================================
;; Diff-HL (Git change indicators in the fringe)
;; =============================================================================

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (conf-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

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

  ;; TS files: typescript-language-server for code actions.
  ;; Angular projects get @angular/language-service injected as a tsserver plugin.
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode typescript-mode)
                 . (lambda (&optional _interactive)
                     (if-let ((project-root (locate-dominating-file default-directory "angular.json")))
                         (let ((node-modules (expand-file-name "node_modules" project-root)))
                           (list "typescript-language-server" "--stdio"
                                 :initializationOptions
                                 `(:plugins [(:name "@angular/language-service"
                                                    :location ,node-modules)])))
                       '("typescript-language-server" "--stdio")))))

  ;; HTML files: Angular LS for template → TS navigation in Angular projects,
  ;; plain typescript-language-server otherwise.
  (add-to-list 'eglot-server-programs
               '((html-ts-mode mhtml-mode)
                 . (lambda (&optional _interactive)
                     (if-let ((project-root (locate-dominating-file default-directory "angular.json")))
                         (let* ((node-modules (expand-file-name "node_modules" project-root))
                                (node-bin (executable-find "node"))
                                (ngserver (or (executable-find "ngserver")
                                              (expand-file-name "ngserver" (file-name-directory node-bin)))))
                           (list node-bin ngserver
                                 "--stdio"
                                 "--tsProbeLocations" node-modules
                                 "--ngProbeLocations" node-modules))
                       '("typescript-language-server" "--stdio")))))

  ;; Show documentation on hover (like VS Code)
  (setq eldoc-echo-area-use-multiline-p nil))  ; Keep it concise

;; =============================================================================
;; Eldoc-box (floating documentation popups for eglot)
;; =============================================================================

(use-package eldoc-box
  :vc (:url "https://github.com/casouri/eldoc-box" :branch "main")
  :after eglot
  :config
  (evil-define-key 'normal eglot-mode-map (kbd "K") #'eldoc-box-eglot-help-at-point))

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
