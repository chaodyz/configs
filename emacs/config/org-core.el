;;; org-core.el --- Core Org mode configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains core Org mode settings:
;; - Basic org configuration (directories, files, mobile sync)
;; - Org babel for code execution
;; - Org tempo for quick templates
;; - Org inlinetask

;;; Code:

;; =============================================================================
;; Core Org Configuration
;; =============================================================================

  (use-package org
    :ensure t
    :init
    (setq org-ellipsis " ▼"
          org-hide-emphasis-markers t
          org-directory "~/eSync/org/"
          org-default-notes-file "~/eSync/org/index.org")
    (setq org-agenda-files '("~/eSync/org" "~/eSync/org/roam"))
    ;; Set to the name of the file where new notes will be stored
    (setq org-mobile-inbox-for-pull "~/eSync/org/flagged.org")
    ;; Set to <your Dropbox root directory>/MobileOrg.
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (setq org-startup-indented 1))

;; =============================================================================
;; Org Babel
;; =============================================================================

  (use-package org-babel
    :ensure nil ; already built-in
    :defer t ; lazy loading
    :config
    ;; Set default languages for org-babel blocks
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)
       (js . t)
       (typescript . t)
       (css . t)))
    ;; Enable syntax highlighting for code blocks
    (setq org-src-fontify-natively t))

  ;; (use-package ob-js
  ;;   :after org
  ;;   :config
  ;;   ;; Add support for Node.js
  ;;   (setq org-babel-js-cmd "node"))

;; =============================================================================
;; Org Tempo (Quick Templates)
;; =============================================================================

  (use-package org-tempo
    :ensure nil
    :after org
    :config
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
     ;; Define a template for quotes
    (add-to-list 'org-structure-template-alist '("q" . "quote"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;; Org writing experience settings
(setq org-startup-indented t)  ;; Auto-indent headlines and content
(setq org-startup-folded t)    ;; Fold content on startup for better readability

;; =============================================================================
;; Org Inlinetask
;; =============================================================================

;; Create a task from non-heading text, such as a sentence or paragraph
(require 'org-inlinetask)

(provide 'org-core)
;;; org-core.el ends here
