;;; tools/notion/config.el -*- lexical-binding: t; -*-
;;;###if (modulep! :lang org)

;;https://github.com/richardwesthaver/org-notion
;; doesn't work yet
(use-package! org-notion
  :commands (org-notion-mode org-notion-pull org-notion-push org-notion-browse org-notion-search org-notion-get-user)
  :init
  (add-hook 'org-mode-hook #'org-notion-mode))
