;;; theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains theme configuration:
;; - Zenburn theme (dark mode)
;; - Solarized theme (light mode)
;; - Circadian for automatic theme switching based on location

;;; Code:

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

(provide 'theme)
;;; theme.el ends here
