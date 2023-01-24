;;; app/org-notes/roam.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org +roam2)

(after! org-roam
  ;; set the roam directory
  (setq org-roam-directory (concat my/notes-directory "roam"))

  ;;(add-to-list 'org-roam-mode-section-functions #'org-roam-unlinked-references-section)

  ;; Map autoloaded functions to keys
  (map!
   (:map org-mode-map
    "M-]" #'my/org-roam-replace-link
    :localleader
    (:prefix "m"
     "l" #'my/org-roam-replace-and-follow
     "L" #'my/org-roam-replace-link))
   :leader
   (:prefix "n"
    (:prefix "r"
     "t" #'org-roam-dailies-today)))

  (add-hook! 'org-roam-capture-new-node-hook
             #'my/org-roam-add-draft-tag-unless-daily-or-existing))
