;; Font 
;; Set font by different mode: (set-face-attribute 'default nil :font "FONT_NAME":height: FONT_SIZE MODE/BUFFER)
(set-face-attribute 'default nil :font "Fira Code" :height 180)
(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 180 :weight 'normal)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 160)
;; 汉字字体设置为思源黑体
(set-fontset-font t 'han (font-spec :family "Source Han Sans CN"))

;; Enable visible bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)  ; Disable the menu bar

(setq split-width-threshold 160) ; the minimum width of a window that Emacs should split horizontally instead of vertically. 
(setq split-height-threshold 80)

;; -----------------------------------------
;; Initialize package.e
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use package- a popular macro in Emacs that provides declarative way 
;; to config and load packages (eval-when-compile
(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" default))
 '(org-agenda-files
   '("~/org/ukg/oneApp.org" "/Users/di.zhou/org/agenda/tasks.org"))
 '(package-selected-packages
   '(ob-js evil-magit projectile org-roam org-superstar pyim command-log-mode move-lines evil-nerd-commenter general helpful which-key ivy evil magit use-package))
 '(pyim-dicts
   '((:name "lazy" :file "/Users/diz/.emacs.d/pyim/pyim-bigdict.pyim.gz")))
 '(warning-suppress-types '((use-package) (use-package) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch :height 0.9))))
 '(org-code ((t (:inherit (shadow fixed-pitch) :height 0.9))))
 '(org-default ((t (:inherit default :height 1.0))))
 '(org-ellipsis ((t (:inherit default :weight normal :height 1.0 :underline nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
 '(org-link ((t (:inherit link :height 1.0)))))

;; ---------------------  Begin use-package

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))
;; Evil
(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))
(define-key evil-normal-state-map "gc" 'evilnc-comment-or-uncomment-lines)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))
(global-set-key (kbd "C-c t") 'hydra-text-scale/body)
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
  (setq zenburn-use-variable-pitch 0)
  (setq zenburn-scale-org-headlines 0)
  (setq zenburn-scale-outline-headlines 0)
  )

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-high-contrast-mode-line 0)
  )

;; Load Theme by location's sunrise and sunset 
(use-package circadian
  :ensure t
  :config
  ;; Set Toronto as the location for sunrise and sunset times
  (setq calendar-latitude 43.6532
        calendar-longitude -79.3832
        calendar-location-name "Toronto, Canada")
  (setq circadian-themes '((:sunrise . solarized-light-high-contrast)
                           (:sunset  . zenburn)))
  (circadian-setup))

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

(defun describe-thing-at-point ()
  "Show the documentation of the symbol at point."
  (interactive)
  (let ((thing (symbol-at-point)))
    (if thing
        (describe-symbol thing)
      (message "No symbol at point."))))
(global-set-key (kbd "C-c d") 'describe-thing-at-point)


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
    "d" 'describe-thing-at-point
    "f" 'counsel-find-file
    "h" 'counsel-command-history
    "r" 'counsel-recentf
    "w" 'save-buffer
    "q" #'delete-window
    "R" 'eval-last-sexp
    )

  ;; Key-Buffer
  (leader-key-def
    "b" '(:ignore t:which-key "Buffer")
    "b l" 'counsel-ibuffer
    "b c" 'kill-buffer
    "b w" 'save-buffer 
    )
   ;; Key-Org
  (leader-key-def
    "o" '(:ignore t:which-key "Org")
    "o a" 'org-agenda
    "o s" 'org-schedule
    "o d" 'org-deadline
    "o c" 'org-capture
    )
  ;; Key-pyim
  (leader-key-def
    "p" '(:ignore t:which-key "pyim")
    "p c" 'pyim-punctuation-translate-at-point
    )
   ;; Window
  (leader-key-def
    "a" '(:ignore t:which-key "Window")
    "a v" 'split-window-right
    "a s" 'split-window-below
    ;; window movement binding
    "a w" #'other-window
    "a h" #'evil-window-left
    "a j" #'evil-window-down
    "a k" #'evil-window-up
    "a l" #'evil-window-right
    "a H" #'evil-window-move-far-left
    "a J" #'evil-window-move-very-bottom
    "a K" #'evil-window-move-very-top
    "a L" #'evil-window-move-far-right)

  (leader-key-def
            "g" '(:ignore t :which-key "Magit")
            "g s" 'magit-status
            "g b" 'magit-blame
            "g l" 'magit-log-buffer-file
            "g g" 'magit-dispatch
            "g c" 'magit-commit-create)
)


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

;; 拼音
(use-package pyim
  :ensure nil
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))
  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。

 (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; ;; 使用 pupup-el 来绘制选词框
  ;; (setq pyim-page-tooltip 'popup)
  ;; (setq pyim-page-tooltip 'pos-tip)

  ;; 选词框显示5个候选词
  ;; (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind

  (
   ("M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
  ;; ("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))


(column-number-mode)
;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
; Wrap text 
(global-visual-line-mode t)

;; Org Mode
(use-package org
  :ensure t
  :init
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t
        org-directory "~/org/"
        org-default-notes-file "~/org/index.org")
  (setq org-agenda-files '("~/org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-startup-indented t)
  :hook (org-mode . my-org-mode-setup)
  :config
;; Configure org mode to start with modes that more visual appealing
;; - visual-line-mode: wraps lines at window width for easy reading and editing
;; - variable-pitch-mode 1: sets the font face to a variable-width font for a more natural and aesthetically pleasing look
  (defun my-org-mode-setup ()
    "Setup visual line and variable pitch modes for Org mode."
    (visual-line-mode)  
    (variable-pitch-mode 1) 
    )
  ;; Other Org mode configurations here...
  ;; Set faces for headings, lists, and other elements
  (custom-set-faces
  ;; Set font and size for headlines
  '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.12))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.09))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.06))))
  '(org-default ((t (:inherit default :height 1.0))))
  '(org-block ((t (:inherit fixed-pitch :height 0.9))))
  '(org-code ((t (:inherit (shadow fixed-pitch) :height 0.9))))
  '(org-link ((t (:inherit link :height 1.0))))
  '(org-ellipsis ((t (:inherit default :weight normal :height 1.0 :underline nil)))))


    (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))
;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/org/journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/org/journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/org/journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/org/metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

)

;; Make sure org-indent face is available
(require 'org-indent)
;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil) (set-face-attribute 'org-column-title nil :background nil)

;; headline bullet
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("☵" "○" "✻" "✿"))
  :config
  (set-face-attribute 'org-superstar-item nil :height 1.0))

;; cosmetic function
(defun my/org-mode-hook ()
  "Customize Org mode settings."
  (setq-default line-spacing 0.2)
  (setq-default org-blank-before-new-entry '((heading . auto)
                                             (plain-list-item . auto))))
(add-hook 'org-mode-hook #'my/org-mode-hook)


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

(use-package ob-js
  :after org
  :config
  ;; Add support for Node.js
  (setq org-babel-js-cmd "node"))

(use-package org-tempo
  :ensure nil
  :hook (org-mode . org-tempo-mode)
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

;; Helper emphasis (ChatGPT) 🤯
(defun my-wrap-with-stars ()
  "Wrap visual selection with *."
  (interactive)
  (let ((selection (buffer-substring-no-properties
                    (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat "*" selection "*"))))

(defun my-wrap-with-tides ()
  "Wrap visual selection with ~."
  (interactive)
  (let ((selection (buffer-substring-no-properties
                    (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat "~" selection "~"))))

(defun my-wrap-with-equals ()
  "Wrap visual selection with =."
  (interactive)
  (let ((selection (buffer-substring-no-properties
                    (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat "=" selection "="))))

;; Bind the function to a key combination
(define-key evil-visual-state-map (kbd "C-*") 'my-wrap-with-stars)
(define-key evil-visual-state-map (kbd "C-~") 'my-wrap-with-tides)
(define-key evil-visual-state-map (kbd "C-=") 'my-wrap-with-equals)

;; Surround with ANY KEY (chatGPT)
(defun surround-with-key (beg end key)
  "Surround the region between BEG and END with KEY."
  (interactive "r\nsSurround with: ")
  (goto-char end)
  (insert key)
  (goto-char beg)
  (insert key))

(general-define-key
 :states '(visual)
 :keymaps 'override
 "s" 'surround-with-key)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/")
  (org-roam-db-location "~/org/org-roam.db")
  (org-roam-completion-everywhere t)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))


;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/projects"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
  :config (conunsel-projectile-mode))

;; Magit
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :config
  (use-package evil-magit
    :ensure t
    :config
    (setq evil-magit-state 'normal) ; set the initial state to normal
    (add-hook 'magit-mode-hook 'evil-magit-init))
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  )
