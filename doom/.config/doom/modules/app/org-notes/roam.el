;;; app/org-notes/roam.el -*- lexical-binding: t; -*-
;;;###if (featurep! :lang org +roam2)

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

  ;; if popups are enabled, add function for opening daily note as popup buffer
  (when (featurep! :ui popup)
    (defun my/roam-daily-as-popup ()
      "Open the daily roam file as a popup buffer."
      (interactive)
      (org-roam-dailies-goto-today)
      (call-interactively #'+popup/buffer)
      (set-window-parameter (selected-window) 'autosave 't))
    ;; bind it
    (map!
     :leader
     (:prefix "n"
      ;; leader-x is the normal scratch buffer
      "x" #'my/roam-daily-as-popup))))
