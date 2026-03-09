;;; editing.el --- Text editing configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains text editing enhancements:
;; - undo-tree for better undo/redo
;; - Evil mode for Vim emulation
;; - Evil collection for Evil mode compatibility
;; - Evil nerd commenter for commenting
;; - Hydra for text scaling
;; - French typing support (right Option key)


;; VIM key repeat settings:
;; Linux
;; Configure to 180ms delay and 50 repeats/second 

;; macOS
;; defaults write -g InitialKeyRepeat -int 12
;; defaults write -g KeyRepeat -int 1

;;; Code:
(setq show-paren-delay 0) ;; Remove delay for showing matching parentheses
(show-paren-mode 1) ;; Highlight matching parentheses

;; Improve Terminal Emacs with system clipboard integration
(setq select-enable-clipboard t) ;; Enable system clipboard integration
(setq select-enable-clipboard t) ;; Enable system clipboard integration

;; =============================================================================
;; Undo Tree
;; =============================================================================

(use-package undo-tree
  :ensure t
  :init
  :config
  (global-undo-tree-mode 1))

;; =============================================================================
;; Evil Mode (Vim Emulation)
;; =============================================================================


(use-package evil
  :init
  (setq evil-undo-system 'undo-tree);; tell Evil to use undo-tree
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  )


(use-package evil-nerd-commenter
  ;; :config
  ;; (evilnc-default-hotkeys)
  )
(define-key evil-normal-state-map "gc" 'evilnc-comment-or-uncomment-lines)

(use-package evil-collection
  :after evil
  :config
  (delete 'magit evil-collection-mode-list) ;; defer magit to avoid load-order issues
  (evil-collection-init))

;; Init magit evil bindings only after magit actually loads
(with-eval-after-load 'magit
  (evil-collection-init '(magit magit-todos)))

;; Escape key to quit menu
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ;; Distinguish C-i from TAB in GUI Emacs so evil-jump-forward works
;; (when (display-graphic-p)
;;   (define-key input-decode-map "\C-i" [C-i])
;;   (with-eval-after-load 'evil
;;     (define-key evil-motion-state-map [C-i] 'evil-jump-forward)))

;; Xref keybindings for Evil mode
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "gd") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "gr") #'xref-find-references))

;; =============================================================================
;; Hydra (Text Scaling)
;; =============================================================================


(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))
(global-set-key (kbd "C-c t") 'hydra-text-scale/body)

;; =============================================================================
;; Indentation & Formatting
;; =============================================================================

;; Set bash indentation to 2 spaces
(setq sh-basic-offset 2)
;; Use spaces instead of tabs (applies to all modes by default)
(setq-default indent-tabs-mode nil)

;; =============================================================================
;; Other Experiments / Settings
;; =============================================================================

;; French typing experience
(setq mac-right-option-modifier nil) ;; Right Option behaves as Option

;; TODO: Try to mimic move lines action
;; keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
;; keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
;; keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
;; keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

(provide 'editing)
;;; editing.el ends here
