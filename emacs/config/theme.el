;;; theme.jl --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains theme configuration:
;; - GUI: doom-nord
;; - Terminal: circadian with solarized-light (sunrise) and zenburn (sunset)

;;; Code:

;; Theme
(use-package zenburn-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-high-contrast-mode-line 0)
  )

(use-package nord-theme
  :ensure t)

(use-package doom-themes
  :ensure t)

(use-package everforest-theme
  :ensure t
  :vc (:url "https://github.com/Theory-of-Everything/everforest-emacs.git" :rev :newest)
  )


(if (display-graphic-p)
    (load-theme 'doom-nord t)
  (use-package circadian
    :ensure t
    :config
    (setq calendar-latitude 43.6532
          calendar-longitude -79.3832
          calendar-location-name "Toronto, Canada")
    (setq circadian-themes '((:sunrise . solarized-light-high-contrast)
                             (:sunset  . zenburn)))
    (circadian-setup)))

(provide 'theme)
;;; theme.el ends here
