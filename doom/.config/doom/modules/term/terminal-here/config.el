;;; term/terminal-here/config.el -*- lexical-binding: t; -*-

(use-package! terminal-here
  :commands (terminal-here-launch terminal-here-project-launch)
  :init
  (setq terminal-here-terminal-command
        (list
         (or (getenv "TERMINAL") "alacritty"))))
