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


(use-package! chatgpt-shell
  :defer t
  :commands (chatgpt-shell chatgpt-shell-explain-code chatgpt-shell-chatgpt-prompt chatgpt-shell-send-region chatgpt-shell-send-and-review-region)
  :config
  (setq chatgpt-shell-model-version "gpt-4")
  (when (modulep! :lang org)
    (add-hook 'org-mode-hook (lambda () (require 'ob-chatgpt-shell)))))

(use-package! dall-e-shell
  :commands (dall-e-shell))
