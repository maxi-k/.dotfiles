;;; tools/openai/config.el -*- lexical-binding: t; -*-

;; https://github.com/karthink/gptel/
(use-package! gptel
  :defer t
  :commands (gptel gptel-send gptel-send-menu)
  :init
  (defvar openai-prompt-history '())
  (defvar openai-model-list '("gpt-3.5-turbo" "gpt-4" "code-davinci-002"))
  (defvar openai-default-model (car openai-model-list))
  (defun openai-reset-prompt-history ()
    (interactive)
    (setq openai-prompt-history '()))
  (if (bound-and-true-p savehist-loaded)
      (add-to-list 'savehist-additional-variables 'openai-prompt-history)
    (defvar savehist-additional-variables nil)
    (add-hook 'savehist-mode-hook
              (lambda ()
                (add-to-list 'savehist-additional-variables 'openai-prompt-history)))))

