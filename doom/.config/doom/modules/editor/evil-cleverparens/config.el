;;; lang/evil-cleverparens/config.el -*- lexical-binding: t; -*-
;;;
(when (featurep! :editor evil)
  (use-package! evil-cleverparens
    :commands evil-cleverparens-mode))
