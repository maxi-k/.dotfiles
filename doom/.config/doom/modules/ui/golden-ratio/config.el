;;; ui/golden-ratio/config.el -*- lexical-binding: t; -*-

(use-package! golden-ratio
  :commands (golden-ratio-mode golden-ratio))

(defun +golden-ratio-toggle ()
  "Toggle golden ratio mode. If toggled on, adjust the window sizes immediately."
  (interactive)
  (if golden-ratio-mode
      (progn
        (golden-ratio-mode -1)
        (message "Golden Ratio disabled."))
    (progn
      (golden-ratio-mode +1)
      (golden-ratio)
      (message "Golden Ratio enabled and applied."))))
