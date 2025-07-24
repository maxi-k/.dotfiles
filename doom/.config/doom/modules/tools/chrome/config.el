;;; tools/chrome/config.el -*- lexical-binding: t; -*-

(use-package! atomic-chrome
  :demand t
  :commands (atomic-chrome-start-server)
  :config
  (setq-default atomic-chrome-extension-type-list '(atomic-chrome))
  (atomic-chrome-start-server))
