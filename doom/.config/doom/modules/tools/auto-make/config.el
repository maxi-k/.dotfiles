;;; tools/auto-make/config.el -*- lexical-binding: t; -*-

(defvar-local auto-make-make-command "make"
  "Command to use for auto export on file save.")

(defvar auto-make-make-async-p t
  "Whether to export asynchronously when auto-exporting on file save.")

(define-minor-mode auto-make-mode
  "Automatically export the current org file on save. Local mode.
Set the export command to be used with `org-auto-export-command`."
  :global nil
  :lighter "auto-export"
  (if auto-make-mode
      ;; enabled
      (add-hook! 'after-save-hook :local #'auto-make-run-make)
      ;; disabled
      (remove-hook! 'after-save-hook :local #'auto-make-run-make)))
