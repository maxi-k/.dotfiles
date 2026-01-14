;;; tools/ai/config.el -*- lexical-binding: t; -*-

(use-package! eca
  :init
  (setq eca-chat--last-known-model "github-copilot/gpt-5-mini")
  :commands (eca-transient-menu eca eca-workspace eca-restart)
  :defer t)
