;;; ui/org-dashboard-roam/config.el -*- lexical-binding: t; -*-
;;;###if (modulep! :ui org-dashboard)

(defgroup org-dashboard-roam nil
  "Options for org-dashboard specific to org-roam"
  :group 'org-dashboard)

(defcustom org-dashboard-roam-tag-exclude-list '()
  "Tags that should be excluded in `org-dashboard-roam-tags' by default."
  :group 'org-dashboard-roam)


(defcustom org-dashboard-roam-tag-emoji-alist '(("cloud" . "☁")
                                                ("idea" . "💡")
                                                ("movie" . "🎞")
                                                ("recipe" . "🍖")
                                                ("paper" . "🎓")
                                                ("person" . "👱")
                                                ("reference" . "📄")
                                                ("research" . "🎓")
                                                ("project" . "⚒"))
  "Mapping emoji to tags for more colorful tag headings"
  :group 'org-dashboard-roam)
;;(setq org-dashboard-roam-tag-emoji-alist (eval (car (get 'org-dashboard-roam-tag-emoji-alist 'standard-value))))

(defcustom org-dashboard-roam-tag-format-string "%s [[elisp:(org-roam-node-find nil \"#%s \")][%s%s]] (%s)"
  "Format string for the headlines inserted by `org-dashboard-roam-tags'.
Formatted with args: stars tag emoji tag count. XXX make this use named format strings; how does org-roam-capture-templates do it?"
  :group 'org-dashboard-roam)

(defun org-dashboard-roam-tags (recur params)
  (let* ((ignored org-dashboard-roam-tag-exclude-list)
         (ignored (append ignored (mapcar (lambda (s) (if (symbolp s) (symbol-name s) s)) (plist-get params :exclude))))
         (ignored (apply #'vector ignored))
         (emoji-only (plist-get params :emoji-only))
         (having (when-let ((min-cnt (plist-get params :more-than)))
                   (list :having `(> cnt ,min-cnt))))
         (and-expr (when-let ((filter-list (plist-get params :only)))
                     (list :and `(in tag [,@(mapcar (lambda (s) (if (symbolp s) (symbol-name s) s)) filter-list)]))))
         (expr `[:select [tag (funcall count *)] :as cnt
                  :from tags
                  :where (not (in tag $v1))
                  ,@and-expr
                  :group-by tag
                  ,@having
                  :order-by cnt :desc])
         (tags (org-roam-db-query expr ignored))
         (stars (make-string (+ 1 (org-current-level)) ?*)))
    (dolist (row tags)
      (pcase-let* ((`(,tag ,count) row)
                   (emoji (alist-get tag org-dashboard-roam-tag-emoji-alist nil nil #'equal))
                   (heading
                    (format org-dashboard-roam-tag-format-string
                                    stars
                                    tag
                                    (if emoji (concat emoji " ") "")
                                    tag
                                    count)))
        (when (or emoji (not emoji-only))
            (insert heading)
            (insert "\n"))))))

