;;; ui/org-dashboard/config.el -*- lexical-binding: t; -*-
;; (defface org-dashboard-title-face
;;   '((default :inherit italic)
;;     (((class color) (min-colors 88) (background light))
;;      :foreground "#904200")
;;     (((class color) (min-colors 88) (background dark))
;;      :foreground "#fba849")
;;     (t :foreground "yellow"))
;;   "Yellow-tinted text with slanted font (italics).")

(defface org-dashboard-heading
`((((class color) (background light))
     :foreground "black")
    (((class color) (background dark))
     :foreground "white"))
  ""
  :group 'org-dashboard-faces)

(defface org-dashboard-title
  `((default :inherit (org-dashboard-heading org-document-title) :weight bold :height 2.5))
  ""
  :group 'org-dashboard-faces)

(defface org-dashboard-h1
  `((default :inherit (org-dashboard-heading org-level-1) :height 2.0))
 "" :group 'org-dashboard-faces)

(defface org-dashboard-h2
  `((default :inherit (org-dashboard-heading org-level-2) :height 1.7 :weight normal))
 ""
 :group 'org-dashboard-faces)

(defface org-dashboard-content
  `((default :inherit org-default :height 1.35))
  ""
  :group 'org-dashboard-faces)

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

(defun org-dashboard-mode--on ()
  (interactive)
  (solaire-mode +1)
  ;;(setq-local org-hide-leading-stars t)
  (setq-local org-hidden-keywords (cons 'title org-hidden-keywords))
  (org-dashboard-reload-faces)
  (add-hook! doom-load-theme-hook :local #'org-dashboard-reload-faces))

(defun org-dashboard-mode--off ()
  (interactive)
  (solaire-mode -1)
  (org-dashboard-reload-faces :reinit nil)
  (remove-hook! doom-load-theme-hook :local #'org-dashboard-reload-faces))

(defun org-dashboard--reload ()
  (interactive)
  (org-dashboard-mode--off)
  (org-dashboard-mode--on))

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
