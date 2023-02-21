;; Font 
;; Set font by different mode: (set-face-attribute 'default nil :font "FONT_NAME":height: FONT_SIZE MODE/BUFFER)
(set-face-attribute 'default nil :font "FiraMono Nerd Font" :height 180)
;; 
;;(load-theme 'tango)

;; Enable visible bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)  ; Disable the menu bar

;; Initialize package.el
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use package- a popular macro in Emacs that provides declarative way 
;; to config and load packages (eval-when-compile
(require 'use-package)

;; Magit
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(command-log-mode move-lines evil-nerd-commenter general helpful which-key ivy evil magit use-package))
 '(warning-suppress-types '((use-package) (use-package) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#5c6370" :slant italic)))))

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
 ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  )

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))
(define-key evil-normal-state-map "gc" 'evilnc-comment-or-uncomment-lines)

;; TODO: Try to mimic move lines action
;; keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
;; keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
;; keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
;; keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)
;; -------------

;; Escape key to quit menu
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Ivy, Ivy-rich, and counsel
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
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

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
  (setq ivy-initial-inputs-alist nil))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Helpful
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; General
(use-package general
  :ensure t
  :config
  (general-create-definer leader-key-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

;; Define some key bindings using the leader key
  (leader-key-def
    "f" 'counsel-find-file
    "r" 'counsel-recentf
    "b" 'counsel-ibuffer
    "c" 'kill-buffer
    "R" 'eval-last-sexp
    "w" 'save-buffer))

;; Which key
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-prefix-prefix "SPC"))


;; Comand log mode
(use-package command-log-mode
  :ensure t)
