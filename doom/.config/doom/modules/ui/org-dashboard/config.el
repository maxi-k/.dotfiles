;;; ui/org-dashboard/config.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org)

;; (defface org-dashboard-title-face
;;   '((default :inherit italic)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#904200")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#fba849")
;;     (t :foreground "yellow"))
;;   "Yellow-tinted text with slanted font (italics).")

(defgroup org-dashboard nil
  "Options for org-dashboard"
  :group 'convenience)

(defface org-dashboard-heading
`((((class color) (background light))
     :foreground "black")
    (((class color) (background dark))
     :foreground "white"))
  ""
  :group 'org-dashboard)

(defface org-dashboard-title
  `((default :inherit (org-dashboard-heading org-document-title) :weight bold :height 2.5))
  ""
  :group 'org-dashboard)

(defface org-dashboard-h1
  `((default :inherit (org-dashboard-heading org-level-1) :height 2.0))
 "" :group 'org-dashboard)

(defface org-dashboard-h2
  `((default :inherit (org-dashboard-heading org-level-2) :height 1.7 :weight normal))
 ""
 :group 'org-dashboard)

(defface org-dashboard-content
  `((default :inherit org-default :height 1.35))
  ""
  :group 'org-dashboard)

(defvar-local org-dashboard--face-remap-fn nil)

(defun org-dashboard--face-transformation ()
  (mapcar (lambda (cs) (face-remap-add-relative (car cs) (cdr cs)))
          '((org-document-title . org-dashboard-title)
            (org-level-1 . org-dashboard-h1)
            (org-level-2 . org-dashboard-h2)
            (org-default . org-dashboard-faces))))

(cl-defun org-dashboard-reload-faces (&key (reinit t) (deinit t))
  (when deinit
    (mapc #'face-remap-remove-relative org-dashboard--face-remap-fn)
    (setq org-dashboard--face-remap-fn nil))
  (when reinit
    (setq org-dashboard--face-remap-fn (org-dashboard--face-transformation))))


(defcustom org-dashboard-dynamic-content-property-name "dashboard-content"
  "The property name to used for dynamic content."
  :group 'org-dashboard)

(defcustom org-dashboard-dynamic-content-id-property-name "dashboard-content-id"
  "The property name used to uniquely identify dynamic content.
Only manually added dynamic content generators should have this property."
  :group 'org-dashboard)

(defcustom org-dashboard-dynamic-content-is-read-only t
  "Whether the dynamically generated content should be marked as read-only text."
  :group 'org-dashboard)

(defcustom org-dashboard-fold-dynamic-content t
  "Whether the dynamic content should be folded (hidden) by default"
  :group 'org-dashboard)

(defcustom org-dashboard-fold-property-name "dashboard-content-fold"
  "Org property keyword indicating whether the dynamic content should be hidden (folded) by for the"
  :group 'org-dashboard)

(defcustom org-dashboard-solaire-mode nil
  "Whether to use `solaire-mode' for the dashboard buffer when enabling org-dashboard-mode"
  :group 'org-dashboard)

(defcustom org-dashboard-dynamic-content-face 'org-block
  "Face to use for dynamic content regions (or nil to not set any face)"
  :group 'org-dashboard)

(defun org-dashboard-parse-dynamic-content (content)
  "Parses a string representing a dynamic content function of the form \"content-fn arg1 arg2...\".
Returns a plist (:fn [function symbol] :args (list of args)). "
  (let* ((parts (read (concat "(" content ")")))
         (fn (car parts))
         (alt-fn (intern (concat "org-dashboard-" (symbol-name fn))))
         (fn (if (fboundp fn) fn (if (fboundp alt-fn) alt-fn (user-error "neither %s nor %s are defined functions" fn alt-fn)))))
    (list :fn fn :args (cdr parts))))

(defun org-dashboard-spec-at-point ()
  "Returns the dashboard spec at point.
A dashboard spec has the shape (:point [point at property] :fn [dynamic content function] :args [dynamic content args] :id [unique content id] :fold [bool-like]) "
  (interactive)
  (let* ((pos (point))
         (content-id-key org-dashboard-dynamic-content-id-property-name)
         (content-key org-dashboard-dynamic-content-property-name)
         (content-id (org-entry-get pos content-id-key))
         (do-fold (org-not-nil (or (org-entry-get pos org-dashboard-fold-property-name t t) org-dashboard-fold-dynamic-content)))
         (dynamic-spec (org-dashboard-parse-dynamic-content (org-entry-get pos content-key))))
    (org-combine-plists (list :point pos :id content-id :fold do-fold) dynamic-spec)))

(defun org-dashboard-collect-specs (&optional beg end)
  "Collect information about entries in the buffer where the `org-dashboard-dynamic-content-property-name' property is set.
Limits search to range between `beg' and `end' if non-nil.
Adds a unique id to the property with key `org-dashboard-dynamic-content-id-property-name' if it is not set yet.
Returns a list of spec plists as defined by `org-dashboard-spec-at-point'."
  (org-with-wide-buffer ;; adapted from `org-property-values'
   (save-excursion
     (let* ((beg (or beg (point-min)))
            (end (or end (point-max)))
            (case-fold-search t)
            (content-key org-dashboard-dynamic-content-property-name)
            (re (org-re-property content-key))
            values)
       (goto-char beg)
       (while (re-search-forward re end t)
         (push (org-dashboard-spec-at-point) values))
       (setq values ;; sort list back-to-front to ensure buffer modifications don't destroy :point
             (sort (delete-dups values)
                   (lambda (x y) (> (plist-get x :point) (plist-get y :point)))))))))

(defun org-dashboard-generate-dynamic-content (content &optional recur)
  "Given a dynamic-content-describing plist as returned by `org-dashboard-collect-specs', renders the dynamic content into the buffer.
Expands dynamically expanded content recursively. It is the responsibility of content functions to ensure recursion is not infinite."
  (let* ((recur (or recur 0))
         (fn (plist-get content :fn))
         (args (plist-get content :args))
         (pos (plist-get content :point))
         (foldp (plist-get content :fold))
         (prev-content))
    (goto-char pos)
    (org-back-to-heading)
    (let* ((elem (org-element-at-point))
           (end (org-element-property :end elem))
           (drawer-end (re-search-forward "^[ \t]*:END:.*" end t))
           (beg (+ drawer-end 1))
           (inhibit-read-only t)
           (insert-start)
           (insert-end))
      ;;(message "found %s \nfrom %s %s" elem beg end)
      (setq prev-content (buffer-substring beg end))
      (condition-case expansion-error
          (progn
            ;; clear previous results
            (when (and beg end) (delete-region beg end))
            (goto-char beg)
            (insert "\n\n")
            (forward-line -1)
            (setq insert-start (point))
            (funcall fn recur args)
            (setq insert-end (point))
            ;; recurse
            (let ((props (org-dashboard-collect-specs insert-start insert-end)))
              (dolist (prop props)
                (org-dashboard-generate-dynamic-content prop (+ recur 1))))
            (setq insert-end (point))
            (save-excursion
              (goto-char pos)
              (org-back-to-heading)
              (setq insert-end (org-element-property :end (org-element-at-point)))
              (when foldp
                (org-fold-hide-subtree)
                (org-fold-show-entry)
                (org-fold-show-children))
              (when (zerop recur)
                (org-fold--hide-drawers pos insert-end)
                (when org-dashboard-dynamic-content-is-read-only
                  ;;(message "marking as read-only")
                  (add-text-properties beg insert-end '(read-only t)))
                ;;(when org-dashboard-dynamic-content-face (add-text-properties beg insert-end `(face highlight)))
                )))
        (error ;; XXX doesn't catch all error cases
         (message "Caught error \"%s\", restoring content at %s..." expansion-error beg)
         (let ((del-end (or insert-end end)))
           (delete-region beg del-end))
         (goto-char beg)
         (insert prev-content)
         )))))

(defun org-dashboard-test-content (recur args)
  (let* ((level (+ 1 (org-current-level)))
         (stars (make-string level ?*))
         (fold (if (>= recur 1) "nil" t))
         (res (if (>= recur 2)
                  (format "%s %s" recur args)
                (format "%s Headline %s.1
:PROPERTIES:
:dashboard-content: test-content recursion1
:dashboard-content-fold: %s
:END:
some content %s
%s Headline %s.2
:PROPERTIES:
:dashboard-content: test-content recursion2
:dashboard-content-fold: %s
:END:
some content %s
%s Headline %s.3
some content %s
" stars recur fold args stars recur fold args stars recur args)
                ))
         )
    (insert res)))

(defun org-dashboard-setup-buffer ()
  (let ((window (get-buffer-window))
        ;; only use entries with an ID value on first level to avoid looping over
        ;; auto-generated recursive entries that will be deleted anyway
        (props (seq-filter (lambda (spec) (plist-get spec :id)) (org-dashboard-collect-specs))))
    ;;(dolist (spec props) (message "found %s" spec))
    (dolist (spec props)
      ;;(message "inserting content for %s" spec)
      (org-dashboard-generate-dynamic-content spec))))

(defun org-dashboard-update-buffer ()
  "Update dynamic content patches in the current buffer."
  (interactive)
  (save-excursion
    (org-dashboard-setup-buffer)))

(defun org-dashboard-update-at-point ()
  "Update the dynamic content on the current point."
  (interactive)
  (save-excursion
    (let ((spec (org-dashboard-spec-at-point)))
      (org-dashboard-generate-dynamic-content spec))))

(defun org-dashboard-mode--on ()
  (interactive)
  (when (and (fboundp solaire-mode) org-dashboard-solaire-mode) (solaire-mode -1))
  ;;(setq-local org-hide-leading-stars t)
  (setq-local org-hidden-keywords (delete-dups (cons 'title org-hidden-keywords)))
  ;;(read-only-mode)
  (org-dashboard-reload-faces)
  (org-dashboard-setup-buffer)
  (font-lock-flush)
  (add-hook! after-save-hook :local #'org-dashboard-setup-buffer)
  (add-hook! doom-load-theme-hook :local #'org-dashboard-reload-faces))

(defun org-dashboard-mode--off ()
  (interactive)
  (when (and (fboundp solaire-mode) org-dashboard-solaire-mode) (solaire-mode -1))
  ;;(read-only-mode)
  (org-dashboard-reload-faces :reinit nil)
  (org-mode)
  (remove-hook! after-save-hook :local #'org-dashboard-setup-buffer)
  (remove-hook! doom-load-theme-hook :local #'org-dashboard-reload-faces))

(define-minor-mode org-dashboard-mode
  "Automatically export the current org file on save. Local mode.
Set the export command to be used with `org-auto-export-command`."
  :global nil
  :lighter "org-dashboard"
  (if org-dashboard-mode
      ;; enabled
      (org-dashboard-mode--on)
      ;; disabled
      (org-dashboard-mode--off)
      ))

(defun org-dashboard--reload ()
  (interactive)
  (save-window-excursion
    (switch-to-buffer-other-window "home.org")
    (org-dashboard-mode--off)
    (org-dashboard-mode--on)))

;;(org-dashboard--reload)
