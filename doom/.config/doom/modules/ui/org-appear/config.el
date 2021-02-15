;;; ui/org-appear/config.el -*- lexical-binding: t; -*-

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :commands org-appear-mode
  :config
  (setq org-appear-autolinks 't
        org-appear-autosubmarkers 't))
