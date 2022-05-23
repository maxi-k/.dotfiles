;;; app/org-notes/autoload/org.el -*- lexical-binding: t; -*-
;;;###if (featurep! :lang org)

;;;###autoload
(defun visit-notes-buffer ()
  "Visit the main org mode notes buffer."
  (interactive)
  (find-file (expand-file-name +org-capture-notes-file org-directory)))

;;;###autoload
(defun org-directory-files (&optional root)
  "List org files in the given directory.
If no directory is given,assume the current one (`default-directory`)."
  (let ((directory (or root default-directory)))
    (directory-files directory nil ".org")))

;;;###autoload
(defun org-recursive-directory-files (&optional root)
  "List org files in the given directory recursively.
If no directory is given, assume the current one (`default-directory`)."
  (let ((directory (or root default-directory)))
    (directory-files-recursively directory (rx ".org" eos))))

;;;###autoload
(defun my/org-insert-timestamped-list-item (&optional other-time active)
  "Insert a (inactive) timestamped list item with `+org/insert-item-below`.
Use the prefix argument to specify the time.
Use two prefix arguments to insert an active timestamp instead."
  (interactive "P")
  (+org/insert-item-below 1)
  (org-time-stamp (if other-time 1 '(16)) (if active nil 'inactive))
  (insert-char 32)) ;; space afterwards

;;;###autoload
(defun my/org-insert-timestamped-list-item-prompt (&optional active)
  "Insert a (inactive) timestamped list item with `+org/insert-item-below`, prompting for the time.
Use a prefix argument to insert an active timestamp instead."
  (interactive "P")
  (my/org-insert-timestamped-list-item 't active))

;;;###autoload
(defun my/update-agenda-files (&optional rebuild-agenda)
  "Update the `org-agenda-files` variable by adding all org-roam files which contain todo-like strings."
  ;; (with-temp-buffer (org-agenda-mode) org-todo-keywords-for-agenda)
  (interactive)
  (let* ((todo-strings '("TODO" "WAIT" "HOLD" "IDEA" "STRT" "PROJ" "\\[ \\]" "\\[?\\]" "\\[X\\]"))
         (args (mapcar (lambda (s) (concat "-e '" s "' ")) todo-strings))
         (search-dir my/notes-directory)
         (default-directory search-dir)
         (exec (executable-find "rg"))
         (cmd (apply #'concat "rg " "--fixed-strings " "--files-with-matches " args))
         (original-agenda org-agenda-files))
    (if exec
        (async-start
         `(lambda ()
            (require 'subr-x)
            (remq nil
                  (mapcar
                   (lambda (f) (when (and f (not (string-empty-p f)) (string-suffix-p ".org" f))
                                 (concat ,search-dir f)))
                   (split-string
                    (shell-command-to-string ,cmd) "\n"))))
         `(lambda (result)
            (if (boundp 'my/original-org-agenda-files)
                ;; TODO duplicates as in ~/foo and /home/user/foo are still possible; how to fix best?
                (setq org-agenda-files (delete-dups (append my/original-org-agenda-files result)))
              (setq org-agenda-files (delete-dups (append org-agenda-files result)))
              (setq my/original-org-agenda-files '(,@original-agenda))
              (when ,rebuild-agenda
                (message "Rebuilding all agenda buffers after sync with roam files...")
                (org-agenda-redo-all 't)))))
      (message "Couldn't find ripgrep for smart agenda file initialization."))))

;;;###autoload
(defun my/update-agenda-on-save ()
  "Update org-agenda-files if in org mode"
  (when (eq major-mode 'org-mode)
    (my/update-agenda-files 't)))
