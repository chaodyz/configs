;;; fonts.el --- Font configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains font configuration for:
;; - English fonts (FiraCode Nerd Font)
;; - Chinese fonts (Source Han Sans)
;; - Fixed-pitch and variable-pitch faces

;;; Code:

;; =============================================================================
;; English Fonts
;; =============================================================================

;; Font
;; Set font by different mode: (set-face-attribute 'default nil :font "FONT_NAME" :height FONT_SIZE MODE/BUFFER)
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 180)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono" :height 160)
(set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 180 :weight 'normal)

;; =============================================================================
;; Chinese Fonts (汉字)
;; =============================================================================

;; 汉字－思源黑体
(set-fontset-font t 'han (font-spec :family "Source Han Sans SC VF"))
(set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 180 :weight 'normal)

(provide 'fonts)
;;; fonts.el ends here
