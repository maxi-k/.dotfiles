;;; tools/translation/config.el -*- lexical-binding: t; -*-

(use-package! go-translate
  :commands (gts-do-translate gts-translate gts-translator)
  :config
  (setq gts-translate-list '(("en" "de"))
        gts-default-translator (gts-translator
                                :picker (gts-prompt-picker :texter (gts-current-or-selection-texter))
                                :engines (list (gts-google-engine))
                                :render (gts-buffer-render))
        gts-buffer-follow-p t)
  (add-hook 'gts-after-buffer-prepared-hook #'evil-motion-state))

(defun +translate/online ()
  "Translate a prompted string or region using an online api."
  (interactive)
  (gts-do-translate))
