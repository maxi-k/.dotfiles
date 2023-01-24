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

(defun org-dashboard-roam-symbol-as-string (s)
  (if (symbolp s) (symbol-name s) s))

(defun org-dashboard-roam-symbols-as-strings (symlist)
  (mapcar #'org-dashboard-roam-symbol-as-string symlist))

(defun org-dashboard-roam-tags (recur params)
  "Insert headlines for tags stored in the roam database.
Each headline links to an elisp function that opens a roam search for nodes with that tag.
Uses emoji from `org-dashboard-roam-tag-emoji-alist' to prefix inserted headlines with.
Params is expected to be a valid plist with the following keys (all optional):

:more-than [int]
        only include tags that have more than N nodes are associated with them (default: 0)
:only [list-of-symbol-or-string]
        only include tags in this list
:exclude [list-of-symbol-or-string]
        exclude tags from this list. combined with `org-dashboard-roam-tag-exclude-list'
:emoji-only [bool]
        only include tags in the output for which `org-dashboard-roam-tag-emoji-alist' has an entry
"
  (let* ((ignored org-dashboard-roam-tag-exclude-list)
         (ignored (append ignored (org-dashboard-roam-symbols-as-strings (plist-get params :exclude))))
         (ignored (apply #'vector ignored))
         (emoji-only (plist-get params :emoji-only))
         (having (when-let ((min-cnt (plist-get params :more-than)))
                   (list :having `(> cnt ,min-cnt))))
         (and-expr (when-let ((filter-list (plist-get params :only)))
                     (list :and `(in tag [,@(org-dashboard-roam-symbols-as-strings filter-list)]))))
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


(defun org-dashboard-roam-nodes-with-tags (recur params)
  "Insert headlines for nodes in the roam database.
Params is expected to be a valid plist of the following keys (required):

:tags [list-of-lists-of-strings-or-symbols]
        a list of list of tags that formulates a conjunctive normal form of tag selections, e.g:
                ((draft wip) (project idea))
        selects nodes that satisfy (and (or draft wip) (or project idea)),
        e.g., drafts and work-in-progress projects and ideas.

and the following optional keys

:exclude-tags [bool] (default t)
        whether to display the tags of each nodes in the headline
"
  (let* ((stars (make-string (+ 1 (org-current-level)) ?*))
         (exclude-tags (plist-get params :exclude-tags))
         (join-idx 0)
         ;; build list of (tag-join-select-clause . tag-join-clause) cons cells
         (join-exprs (mapcar (lambda (taglist)
                               (setq join-idx (+ join-idx 1))
                               `(,(format "t%i.tag" join-idx)
                                 .
                                 ,(concat
                                   (format "join tags t%i on t%i.node_id = n.id and t%i.tag in (" join-idx join-idx join-idx)
                                   (string-join
                                    (mapcar (lambda (s)
                                              (concat "'\"" (org-dashboard-roam-symbol-as-string s) "\"'"))
                                            taglist)
                                    ", ") ")")))
                             (plist-get params :tags)))
         (expr (concat "select n.id, n.title, " (string-join (mapcar #'car join-exprs) ", ")
                       " from nodes n "
                       (string-join (mapcar #'cdr join-exprs) " ")))
         (nodes (org-roam-db-query expr))
         (alignment (seq-reduce (lambda (m x) (max (length (cadr x)) m)) nodes 1))) ;; find max title length
    (dolist (row nodes)
      (let* ((id (car row))
             (name (cadr row))
             (tags (cddr row))
             (tag-str (if exclude-tags "" (concat (make-string (- alignment (length name)) ?\s) ":" (string-join tags ":") ":"))))
        (insert (format "%s [[id:%s][%s]] %s" stars id name tag-str))
        (insert "\n")))))

