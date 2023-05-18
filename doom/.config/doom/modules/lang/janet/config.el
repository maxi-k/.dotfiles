;;; lang/janet/config.el -*- lexical-binding: t; -*-

(use-package! janet-mode
  :commands janet-mode
  :mode "\\.janet\\'"
  :interpreter "janet")
