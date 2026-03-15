;;; term-config.el --- Terminal configuration -*- lexical-binding: t -*-

;;; Code:

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "bash")
  (setq vterm-max-scrollback 10000))

(provide 'term-config)
;;; term-config.el ends here
