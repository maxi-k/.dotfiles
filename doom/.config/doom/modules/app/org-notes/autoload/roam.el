;;; app/org-notes/autoload/roam.el -*- lexical-binding: t; -*-
;;;###if (featurep! :lang org +roam2)

;;;###autoload
(defun my/org-roam-replace-link (&optional arg)
  "Replace the link at point with a new roam-style link.
  Return 't if it was replaced, nil otherwise"
  (interactive "P")
  (save-excursion
    (let ((context (org-element-context)))
      (pcase (org-element-lineage context '(link) t)
        (`nil nil)
        (link
         (let* ((link-type (org-element-property :type link))
                (link-content (cond ((string-equal "roam" link-type) nil)
                                    ((string-equal "fuzzy" link-type) (org-element-property :raw-link link))
                                    (t (let* ((begin (or (org-element-property :contents-begin link)
                                                         (org-element-property :begin link)))
                                              (end (or (org-element-property :contents-end link)
                                                       (org-element-property :end link))))
                                         (when (and begin end)
                                           (string-trim (buffer-substring-no-properties begin end))))))))
           (when link-content
             (save-match-data
               (if (org-in-regexp org-link-bracket-re 1)
                   (progn
                     (replace-match "")
                     (insert (org-link-make-string (concat "roam:" link-content) link-content)))
                 (message "No link at point"))))
           (when arg
             (org-roam-link-replace-at-point))))))))

;;;###autoload
(defun my/walk-org-links (f)
  "Walk org mode links in a buffer, calling `f` at each point. Does not save the excursion."
  (pcase (org-next-link)
    ("No further link found" nil)
    (res
     (funcall f)
     (my/walk-org-links f))))

;;;###autoload
(defun my/migrate-org-roam-buffer ()
  "Migrate the given buffer to org roam v2 if possible."
  (interactive)
  (save-excursion
    (my/walk-org-links #'my/org-roam-replace-link)
    (org-roam-link-replace-all)))

;;;###autoload
(defmacro my/infile-no-traces (file &rest body)
  `(let ((no-traces|visited-p (get-file-buffer (expand-file-name ,file)))
         to-be-removed)
     (save-window-excursion
       (find-file ,file)
       (setq to-be-removed (current-buffer))
       ,@body)
     (unless no-traces|visited-p
       (kill-buffer to-be-removed))))

;;;###autoload
(defun my/migrate-org-roam-file (file)
  (my/infile-no-traces file
                       (message "Migrating %s" file)
                       (my/migrate-org-roam-buffer)
                       (save-buffer)))

;;;###autoload
(defun my/migrate-org-roam-directory (dir)
  (let ((files (directory-files dir  t "\\.org$")))
    (mapc #'my/migrate-org-roam-file files)))

;;;###autoload
(defun my/org-roam-replace-and-follow ()
  "Replaces the link at point with a roam: style link, then follows it."
  (interactive)
  (my/org-roam-replace-link)
  (let ((org-roam-link-auto-replace nil))
    (org-open-at-point)))

;;;###autoload
(defun my/roam-link-loop--capture-advice (result)
  "Advice for continuing the link loop after calling capture from org-roam-node-insert"
  (advice-remove 'org-capture-finalize #'my/roam-link-loop--capture-advice)
  (setq my/in-roam-link-loop nil)
  (when result ;; capture successful -> insert space
    (move-end-of-line nil) ;; fix for org-capture not positioning cursor properly after inserting text
    (insert 32))
  (my/roam-link-loop)
  result)

;;;###autoload
(defun my/roam-link-loop ()
  "Prompt user for inserting links to roam nodes multiple times."
  (interactive)
  (let ((start-loc (current-buffer)))
    (org-roam-node-insert)
    (if (eq (current-buffer) start-loc)
        (progn
          (insert 32)
          (my/roam-link-loop))
      ;; not in same buffer -> org-capture triggered and `org-roam-insert` returned.
      ;; add advice to continue loop after capture is done
      ;; TODO if another loop is started inside capture buffer, this stops the first loop; track recursion depth?
      (unless (and (boundp 'my/in-roam-link-loop) my/in-roam-link-loop)
        (setq my/in-roam-link-loop 't)
        (advice-add 'org-capture-finalize :filter-return #'my/roam-link-loop--capture-advice)))))

;;;###autoload
(defun my/org-roam-add-draft-tag ()
  "Add a 'draft' tag to the given org mode node or file"
  (interactive)
  (org-roam-tag-add '("draft")))

;;;###autoload
(defun my/org-roam-add-draft-tag-unless-daily ()
  "Add a 'draft' tag to the given org mode node or file unless it's a daily node"
  (interactive)
  (unless (org-roam-dailies--daily-note-p)
    (my/org-roam-add-draft-tag)))

;;;###autoload
(defun my/roam-daily-as-popup ()
  "Open the daily roam file as a popup buffer."
  (interactive)
  (if (featurep! :ui popup)
    (progn
      (org-roam-dailies-goto-today)
      (let ((buf (current-buffer)))
        (bury-buffer)
        (+popup-buffer buf (+popup-make-rule "." '(:size 0.4 :autosave t :quit nil :select t)))
        (goto-char (point-max))))
    (org-roam-dailies-goto-today)))
