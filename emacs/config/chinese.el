;;; chinese.el --- Chinese input method configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains Chinese input configuration:
;; - Pyim (拼音输入法)
;; - Custom dictionaries (lazy dict + sogou dicts)
;; - Smart switching rules for English/Chinese
;;
;; Dictionary setup instructions:
;;
;; 1. 懒人词库 (Lazy Dictionary):
;;    http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz
;;
;; 2. 搜狗词库 (Sogou Dictionaries):
;;    - Download from: https://pinyin.sogou.com/dict/
;;    - Convert using scel2pyim: https://github.com/E-Neo/scel2pyim
;;      $ brew install gcc
;;      $ git clone git@github.com:E-Neo/scel2pyim.git
;;      $ gcc -o scel2pyim scel2pyim.c
;;      $ ./scel2pyim NAME.scel NAME.pyim

;;; Code:

;; =============================================================================
;; Pyim Dictionaries
;; =============================================================================

(setq pyim-dicts
      '((:name "懒人包" :file "~/eSync/pyim/lazy.gz")
        (:name "搜狗－饮食大全（官方推荐）" :file "~/eSync/pyim/food.pyim")))

;; =============================================================================
;; Pyim Configuration
;; =============================================================================

(use-package pyim
  :ensure t
  :config
  ;; Activate basedict 拼音词库
  (use-package pyim-basedict
    :ensure t
    :config (pyim-basedict-enable))

  ;; Pyim probe settings for seamless Chinese/English switching
  ;; Rules:
  ;; 1. Only input Chinese when cursor is in a comment
  ;; 2. Only input Chinese when preceded by a Chinese character
  ;; 3. Use M-j to force convert pinyin string to Chinese

  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; Enable pinyin search functionality
  (pyim-isearch-mode 1)

  ;; Alternative tooltip options (currently disabled):
  ;; (setq pyim-page-tooltip 'popup)
  ;; (setq pyim-page-tooltip 'pos-tip)

  ;; Selection menu options (currently using defaults):
  ;; (setq pyim-page-length 5)

  ;; Auto-load pyim dictionaries on Emacs startup
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))

  :bind
  (
   ;; Force convert pinyin at point to Chinese (works with pyim-probe-dynamic-english)
   ("M-j" . pyim-convert-string-at-point)
   ;; Delete word from personal buffer
   ("C-;" . pyim-delete-word-from-personal-buffer)))

(provide 'chinese)
;;; chinese.el ends here
