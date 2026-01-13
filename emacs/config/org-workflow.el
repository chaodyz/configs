;;; org-workflow.el --- Org mode workflow configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains Org mode workflow features:
;; - Custom tags
;; - TODO keywords and states
;; - Custom agenda views
;; - Capture templates

;;; Code:

;; =============================================================================
;; Custom Tags
;; =============================================================================

(setq org-tag-alist
      '(
        ;; Location
        ("home" . ?h)
        ("out task" . ?o)
        ;; Device/occasion
        ("computer" . ?d)
        ("phone" . ?m)
        ;; Work
        ("work" . ?w)
        ("CP" . ?x)
        ;; Personal
        ("chore" . ?c)
        ("finance" . ?f)
        ("relationship" . ?l)
        ("interview" . ?i)
        ("swim" . ?s)
        ("CG" . ?g)
        ("read" . ?r)
        ;; Status
        ("planning" . ?n)
        ("backlog" . ?b)))

;; =============================================================================
;; Custom TODO Keywords
;; =============================================================================

;; TODO → NEXT → DONE or CANCEL
;; - sequence: Defines the order of states
;; - |: Separates active states (left) from completed/closed states (right)
;; - Shortcut keys: (t), (n), (d), (c) for quickly switching states
;; - Special markers:
;;   @: Prompts for a note when transitioning to this state
;;   !: Records a timestamp when transitioning to this state

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
;; Custom Agenda Views
;; =============================================================================

(setq org-agenda-custom-commands
      '(("p" "Planning"
             ((tags-todo "+planning"
                         ((org-agenda-overriding-header "Planning Tasks")))
              (tags-todo "-{.*}"
                         ((org-agenda-overriding-header "Untagged Tasks")))
              (todo ".*" ((org-agenda-files '("~/eSync/org/tasks.org"))
                          (org-agenda-overriding-header "Unprocessed TODO Items")))))

        ("d" "Daily Agenda"
         ((agenda "" ((org-agenda-span 'day)
                      (org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority Tasks")))))

        ("w" "Weekly Review"
         ((agenda ""
                  ((org-agenda-overriding-header "Completed Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                   (org-agenda-span 'week)))

          (agenda ""
                  ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-span 'week)))))))

;; =============================================================================
;; Capture Templates
;; =============================================================================

(setq org-capture-templates
      '(("t" "Task" entry (file+olp "~/eSync/org/tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("j" "Journal" entry
         (file+olp+datetree "~/eSync/org/journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in :clock-resume :empty-lines 1)

        ("f" "French Class Notes" entry
         (file+olp+datetree "~/eSync/org/french.org")
         "* French Class Notes: %T
** General
%?
** Vocabulary
- Word 1:
- Word 2:
** Grammar
- Rule 1:
- Rule 2:
")))

(provide 'org-workflow)
;;; org-workflow.el ends here
