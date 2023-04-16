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
  :init

  ;; TODO move these to autoload.el without breaking header-line-format :eval
  (defun chatgpt-shell-header-function ()
    (let* ((left (propertize chatgpt-shell-model-version 'face 'bold))
           (right chatgpt-shell-system-prompt)
           (avail-width (- (window-width) (length left)))
           (right (if (>= (+ 5 (length right)) avail-width)
                      (string-truncate-left right (- avail-width 3))
                    right))
           (fmt-str (format "%%s %%%ds" avail-width)))
      (propertize (format fmt-str left right) 'face 'mode-line-inactive)))

  (defun chatgpt-shell-header--off ()
    (interactive)
    (setq header-line-format nil))

  (defun chatgpt-shell-header--on ()
    (interactive)
    (if (eq major-mode 'shell-maker-mode)
        (setq header-line-format '(:eval (funcall #'chatgpt-shell-header-function)))
      (user-error "not in a shell buffer!")))

  (define-minor-mode chatgpt-shell-header-mode
    "Minor mode for displaying model version as a header in the
chatgpt-shell buffer."
    :global nil
    :lighter "chatgpt-shell-header"
    (if chatgpt-shell-header-mode ;; see autoloads
        (chatgpt-shell-header--on)
      (chatgpt-shell-header--off)))

  (advice-add 'chatgpt-shell :after (lambda (&rest args) (chatgpt-shell-header-mode)))
  :config
  ;;(setq chatgpt-shell-model-version "gpt-4")
  (when (modulep! :lang org)
    (add-hook 'org-mode-hook (lambda () (require 'ob-chatgpt-shell)))))

(use-package! dall-e-shell
  :commands (dall-e-shell))
