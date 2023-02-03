;;; tools/auto-make/config.el -*- lexical-binding: t; -*-

(defcustom auto-make-make-command "make"
  "Command to use for auto export on file save."
  :group 'convenience
  :local t
  :type 'string)

(defcustom auto-make-make-async-p t
  "Whether to export asynchronously when auto-exporting on file save."
  :group 'convenience
  :local nil
  :type 'boolean)

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
