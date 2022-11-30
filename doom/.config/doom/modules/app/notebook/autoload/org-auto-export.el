;;; app/notebook/autoload/org-auto-export.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org)

;;;###autoload
(defun org-auto-export-export ()
  (interactive)
  (if (eq major-mode 'org-mode)
      (funcall org-auto-export-command org-auto-export-async-p)
    (message "[org-auto-export]: Not in org mode.")))

;;;###autoload
(defun org-auto-export-set-export-command (fn)
  (interactive
   (list (read-command "Export fn name: ")))
  (setq org-auto-export-command fn))
