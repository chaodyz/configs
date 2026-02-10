;;; fonts.el --- Font configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains font configuration for:
;; - English fonts (FiraCode Nerd Font Mono, Source Sans 3)
;; - Chinese fonts (Source Han Sans SC VF)

;;; Code:

  ;; Font
  ;; Set font by different mode: (set-face-attribute 'default nil :font "FONT_NAME":height: FONT_SIZE MODE/BUFFER)
  (set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height 120)
  (set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 160 :weight 'normal)

;; 汉字－思源黑体
  (set-fontset-font t 'han (font-spec :family "Source Han Sans SC VF"))

(provide 'fonts)
;;; fonts.el ends here
