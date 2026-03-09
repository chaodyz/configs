;;; fonts.el --- Font configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains font configuration for:
;; - English fonts (FiraCode Nerd Font Mono, Source Sans 3)
;; - Chinese fonts (Source Han Sans SC VF)

;;; Code:

;; Font heights — larger on macOS due to higher DPI displays
;; Set before fonts.el loads to override

(defvar my/is-mac (eq system-type 'darwin)                            
  "Non-nil when running on macOS.")                                   
(defvar my/font-default-height (if (eq system-type 'darwin) 150 130)
  "Height for default font.")
(defvar my/font-fixed-height (if (eq system-type 'darwin) 140 120)
  "Height for fixed-pitch font.")
(defvar my/font-variable-height (if (eq system-type 'darwin) 180 160)
  "Height for variable-pitch font.")

(defvar my/font-default-family "FiraCode Nerd Font Mono"
  "Font family used for the default face.")

(defvar my/font-variable-family "Source Sans 3"
  "Font family used for the variable-pitch face.")

(defvar my/font-han-family "Source Han Sans SC VF"
  "Font family used for Han characters.")

(defun my/font-installed-p (font-name)
  "Return non-nil when FONT-NAME is installed."
  (find-font (font-spec :family font-name)))

(defun my/apply-fonts (&optional frame)
  "Apply configured fonts to FRAME when running graphically."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (when (my/font-installed-p my/font-default-family)
        (set-face-attribute 'default nil :font my/font-default-family :height my/font-default-height)
        (set-face-attribute 'fixed-pitch nil :font my/font-default-family :height my/font-fixed-height))
      (when (my/font-installed-p my/font-variable-family)
        (set-face-attribute 'variable-pitch nil :family my/font-variable-family :height my/font-variable-height :weight 'normal))
      (when (my/font-installed-p my/font-han-family)
        ;; 汉字－思源黑体
        (set-fontset-font t 'han (font-spec :family my/font-han-family))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/apply-fonts)
  (add-hook 'emacs-startup-hook #'my/apply-fonts))


(provide 'fonts)
;;; fonts.el ends here
