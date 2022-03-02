;;; tools/org-notes/config.el -*- lexical-binding: t; -*-
;;;###if (featurep! :lang org)

(after! org
  ;; some relevant variables
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-startup-folded 't
        org-duration-format (quote h:mm)
        org-capture-templates
        (append '(("t" "Personal todo" entry
                   (file+headline +org-capture-todo-file "Inbox")
                   "* TODO %?\n" :prepend t)
                  ("f" "File todo" entry
                   (file+headline +org-capture-todo-file "Inbox")
                   "* TODO %?\n%i\n%a" :prepend t))
                (cdr org-capture-templates))
        org-latex-pdf-process '("latexmk -lualatex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
        org-latex-listings 'minted)

  ;; map some of the autoloaded functions
  (map!
   (:map org-mode-map :localleader
    (:prefix "d"
     "l" #'my/org-insert-timestamped-list-item
     "i" #'my/org-insert-timestamped-list-item-prompt)
    (:prefix "m"
     "t" #'my/roam-link-loop)))

  (add-hook! org-agenda-mode
    (unless (boundp 'my/original-org-agenda-files)
      ;; org seems to set the variable to the org-directory if it's empty,
      ;; and then searches this directory; this duplicates some of our logic,
      ;; so set the variable to some file that doesn't exist
      (when (or (not org-agenda-files) (eq (car org-agenda-files) org-directory))
        (setq org-agenda-files (list (concat my/notes-directory ".agenda.org"))))
      (my/update-agenda-files 't)))

  (add-hook! org-mode
    (add-hook! 'after-save-hook :local #'my/update-agenda-on-save)))

;; load the roam config if available
(when (featurep! :lang org +roam2)
  (load! "roam"))
