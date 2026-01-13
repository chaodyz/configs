;;; org-roam.el --- Org Roam configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This module contains Org Roam configuration:
;; - Basic org-roam setup
;; - Custom capture templates for:
;;   - Leetcode problems
;;   - Mistake logs
;;   - Algorithm templates
;;   - Weekly reviews

;;; Code:

;; =============================================================================
;; Org Roam Configuration
;; =============================================================================

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/eSync/org/roam/")
  (org-roam-db-location "~/eSync/org/roam/org-roam.db")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (when (fboundp 'org-roam-setup)
    (org-roam-setup)))

;; =============================================================================
;; Org Roam Capture Templates
;; =============================================================================

(setq org-roam-capture-templates
      `(("l" "Leetcode Problem" plain
         "%?"
         :if-new (file+head "leetcode/${slug}.org"
                            "#+title: ${title}\n#+filetags: :leetcode:\n\n* 📌 Problem Description\n\n* 💡 Solution Summary\n\n* ✅ Java Code\n#+begin_src java\n\n#+end_src\n\n* ❗️ Pitfalls\n\n* 🔁 Related Patterns\n\n* 🧠 Similar Problems\n")
         :unnarrowed t)

        ("w" "Mistake Log" plain
         "* ❗️ Mistake: ${title}\n- Cause:\n- Fix:\n- Correct Solution:\n- Related Notes: [[id:]]\n"
         :if-new (file+head "wrong/${slug}.org"
                            "#+title: Mistake - ${title}\n#+filetags: :wrong:review:\n")
         :unnarrowed t)

        ("a" "Algorithm Template" plain
         "* ✨ Overview\n\n* 🧱 Java Template\n#+begin_src java\n\n#+end_src\n\n* 📌 Key Points\n\n* 🔁 Common Questions\n- [[id:]]\n- [[id:]]\n"
         :if-new (file+head "algo/${slug}.org"
                            "#+title: ${title}\n#+filetags: :algo:template:\n")
         :unnarrowed t)

        ("r" "Weekly Review" plain
         "* ✅ Solved This Week\n- Number of Problems:\n- Patterns Reviewed:\n- Mistakes:\n\n* 🔁 Frequent Patterns\n- [[id:]]\n\n* 🤯 Confusing Points\n\n* 📌 Next Week Goals\n"
         :if-new (file+head "review/${slug}.org"
                            "#+title: Weekly Review ${title}\n#+filetags: :weekly:review:\n")
         :unnarrowed t)))

;; =============================================================================
;; Org Roam UI (Optional - Currently Commented Out)
;; =============================================================================

;; Uncomment to enable org-roam-ui visualization
;; Reference: https://github.com/org-roam/org-roam-ui
;;
;; (use-package org-roam-ui
;;   :straight
;;     (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;     :after org-roam
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

(provide 'org-roam)
;;; org-roam.el ends here
