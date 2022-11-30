;;; app/notebook/config.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org)

(defcustom org-auto-export-command 'org-html-export-to-html
  "Command to use for auto export on file save."
  :group 'convenience
  :local t
  :type 'string)

(defcustom org-auto-export-async-p t
  "Whether to export asynchronously when auto-exporting on file save."
  :group 'convenience
  :local nil
  :type 'boolean)

(define-minor-mode org-auto-export-mode
  "Automatically export the current org file on save. Local mode.
Set the export command to be used with `org-auto-export-command`."
  :global nil
  :lighter "auto-export"
  (if org-auto-export-mode
      ;; enabled
      (add-hook 'after-save-hook #'org-auto-export-export)
      ;; disabled
      (remove-hook 'after-save-hook #'org-auto-export-export)
      ))
