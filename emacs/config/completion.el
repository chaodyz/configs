;;; completion.el --- Completion frameworks configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains completion and search frameworks:
;; - Ivy: Generic completion frontend
;; - Ivy-rich: Enhanced Ivy display
;; - Counsel: Ivy-enhanced commands
;; - Company: In-buffer completion

;;; Code:

;; =============================================================================
;; Ivy, Ivy-rich, and Counsel
;; =============================================================================

   ;; Note: ivy-rich must be setup after Ivy and counsel
   (use-package ivy
     :ensure t
     :diminish ivy-mode
     :bind (("C-s" . swiper)
            :map ivy-minibuffer-map
            ("TAB" . ivy-alt-done)
            ("C-l" . ivy-alt-done)
            ("C-j" . ivy-next-line)
            ("C-k" . ivy-previous-line)
            ("C-v" . ivy-split-vertical)      ; Open in vertical split (right)
            ("C-s" . ivy-split-horizontal)    ; Open in horizontal split (below)
            :map ivy-switch-buffer-map
            ("C-k" . ivy-previous-line)
            ("C-l" . ivy-done)
            ("C-d" . ivy-switch-buffer-kill)
            :map ivy-reverse-i-search-map
            ("C-k" . ivy-previous-line)
            ("C-d" . ivy-reverse-i-search-kill))
     :config
     (ivy-mode 1)

     ;; Define actions for split opening
     (defun my/ivy-open-in-split (x split-fn)
       "Open X in a split created by SPLIT-FN."
       (select-window (funcall split-fn))
       (if (file-exists-p x)
           (find-file x)
         (if (get-buffer x)
             (switch-to-buffer x)
           (find-file x))))

     (defun ivy-split-vertical ()
       "Exit Ivy and open selection in vertical split."
       (interactive)
       (ivy-exit-with-action
        (lambda (x) (my/ivy-open-in-split x #'split-window-right))))

     (defun ivy-split-horizontal ()
       "Exit Ivy and open selection in horizontal split."
       (interactive)
       (ivy-exit-with-action
        (lambda (x) (my/ivy-open-in-split x #'split-window-below)))))

   (use-package ivy-rich
     :ensure t
     :config
     (ivy-rich-mode 1))

   (use-package counsel
     :ensure t
     :bind (("M-x" . counsel-M-x)
            ("C-x b" . counsel-ibuffer)
            ("C-x C-f" . counsel-find-file)
            ("C-c f" . counsel-recentf))
     :config
     (setq ivy-initial-inputs-alist nil)

     ;; Ignore temporary and backup files in counsel-find-file
     (setq counsel-find-file-ignore-regexp
           (concat
            "\\(?:\\`\\.#\\)"           ; Lock files (.#filename)
            "\\|\\(?:\\`#.*#\\'\\)"     ; Auto-save files (#filename#)
            "\\|\\(?:~undo-tree~\\)"))  ; Undo-tree files
     )
;; ============================================================================= ;; Company (In-buffer Completion)
;; =============================================================================

  (use-package company
    :ensure t
    :config
    (global-company-mode 1))

;; =============================================================================
;; iflipb (VS Code-style Buffer Cycling)
;; =============================================================================

  (use-package iflipb
    :ensure t
    :config
    ;; Ignore special/internal buffers when cycling
    (setq iflipb-ignore-buffers '("^\\*" "^magit"))
    ;; Wrap around when reaching end of buffer list
    (setq iflipb-wrap-around t))

  ;; Buffer cycling with Ctrl+Tab (won't conflict with mode-specific Tab)
  ;; Works globally, like VS Code
  (global-set-key (kbd "C-<tab>") 'iflipb-next-buffer)
  (global-set-key (kbd "C-S-<tab>") 'iflipb-previous-buffer)
  (global-set-key (kbd "<C-iso-lefttab>") 'iflipb-previous-buffer)  ; Linux compatibility

  ;; Also add Vim-style gt/gT for buffer switching (traditional Vim keys)
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "gt") 'iflipb-next-buffer)
    (define-key evil-normal-state-map (kbd "gT") 'iflipb-previous-buffer))

(provide 'completion)
;;; completion.el ends here
