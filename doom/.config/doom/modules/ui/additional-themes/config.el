;;; ui/additional-themes/config.el -*- lexical-binding: t; -*-

(use-package! modus-themes :defer t)
(use-package! flexoki-themes
  :defer t
  :custom
  (flexoki-themes-use-bold-keywords t)
  (flexoki-themes-use-bold-builtins t)
  (flexoki-themes-use-italic-comments t))
