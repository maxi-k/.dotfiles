;;; lang/evil-cleverparens/config.el -*- lexical-binding: t; -*-
;;;
(when (modulep! :editor evil)
  (use-package! evil-cleverparens
    :commands evil-cleverparens-mode))
