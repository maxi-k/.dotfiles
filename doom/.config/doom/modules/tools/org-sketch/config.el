;;; tools/org-sketch/config.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org)

(use-package! org-xournalpp
  :config
  (add-hook 'org-mode-hook #'org-xournalpp-mode))
