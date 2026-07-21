;;; term-config.el --- Terminal configuration -*- lexical-binding: t -*-

;;; Code:

(use-package eat
  :ensure t)

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 10000))

(use-package ghostel
  :ensure t)

(provide 'term-config)
;;; term-config.el ends here
