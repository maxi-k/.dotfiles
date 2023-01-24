;;; ui/org-dashboard-roam/config.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui org-dashboard)

(defgroup org-dashboard-roam nil
  "Options for org-dashboard specific to org-roam"
  :group 'org-dashboard)

(defcustom org-dashboard-roam-tag-ignore-vector []
  "Tags that should be ignored in `org-dashboard-roam-tags'."
  :group 'org-dashboard-roam)

(defcustom org-dashboard-roam-tag-emoji-alist '(("cloud" . "☁")
                                                ("idea" . "💡")
                                                ("movie" . "🎞")
                                                ("recipe" . "🍖")
                                                ("paper" . "🎓")
                                                ("person" . "👱")
                                                ("research" . "🎓")
                                                ("project" . "⚒"))
  "Mapping emoji to tags for more colorful tag headings"
  :group 'org-dashboard-roam)

(defun org-dashboard-roam-tags (recur params)
  (let* ((ignored org-dashboard-roam-tag-ignore-vector)
         (ignored (if (vectorp ignored) ignored (apply #'vector ignored)))
         (tags (org-roam-db-query
               [:select [tag (funcall count *)] :as cnt
                :from tags
                :where (not (in tag $v1))
                :group-by tag
                :order-by cnt :desc] ignored))
        (stars (make-string (+ 1 (org-current-level)) ?*)))
    (dolist (row tags)
      (pcase-let* ((`(,tag ,count) row)
                   (heading
                    (format "%s [[elisp:(org-roam-node-find nil \"#%s \")][%s%s]] (%s)"
                                    stars
                                    tag
                                    (if-let ((emoji (alist-get tag org-dashboard-roam-tag-emoji-alist nil nil #'equal)))
                                        (concat emoji " ")
                                      "")
                                    tag
                                    count)))
        (insert heading)
        (insert "\n")))))
