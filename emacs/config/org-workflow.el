;;; org-workflow.el --- Org mode workflow configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains Org mode workflow features:
;; - TODO keywords and states
;; - Capture templates

;;; Code:

;; =============================================================================
;; Custom Tags
;; =============================================================================

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCEL(c@)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "orange" :weight bold))
        ("NEXT" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCEL" . (:foreground "gray" :weight bold))))

(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold))
        (?B . (:foreground "orange" :weight bold))
        (?C . (:foreground "green" :weight bold))))

(setq org-log-done 'time)
(setq org-agenda-start-with-log-mode t)
(setq org-log-into-drawer t)

;; =============================================================================
;; Capture Templates
;; =============================================================================

(setq org-capture-templates
      `(("j" "Journal" entry
         (file+olp+datetree ,my/org-journal-file)
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in :clock-resume :empty-lines 1)))

(provide 'org-workflow)
;;; org-workflow.el ends here
