;;; editor/copilot/config.el -*- lexical-binding: t; -*-

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-RET" . 'copilot-accept-completion)
              ("M-p" . 'copilot-previous-completion)
              ("M-n" . 'copilot-next-completion)
              ("C-c" . 'copilot-clear-overlay)
              ("C-g" . 'copilot-clear-overlay)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
