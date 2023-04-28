;;; tools/openai/config.el -*- lexical-binding: t; -*-
(defvar myai-system-prompts
  `((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely. Always show code snippets in markdown blocks with language labels.")
    (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
    (writing . "You are a large language model and a writing assistant. Respond concisely.")
    (revise . "You are a large language model and a writing assistant. Revise the given text without any additional text, prompt or note. Your response text should change the original meaning as little as possible.")
    (spellcheck . "You are a large language model and a writing assistant. Only correct any spelling mistakes you find. Change the input text as little as possibler. Do not comment on the semantic meaning of the input. If you don't find any mistakes then reply with \"No mistakes found!\". Respond consicely.")
    (chat . "You are a large language model and a conversation partner. Respond concisely.")))

(defvar myai-model-versions '("gpt-3.5-turbo" "gpt-4"))
(defvar myai-prompt-history '())

(if (bound-and-true-p savehist-loaded)
    (add-to-list 'savehist-additional-variables 'myai-prompt-history)
  (defvar savehist-additional-variables nil)
  (add-hook 'savehist-mode-hook
            (lambda ()
              (add-to-list 'savehist-additional-variables 'myai-prompt-history))))

(use-package! chatgpt-shell
  :defer t
  :commands (chatgpt-shell
             chatgpt-shell-explain-code
             chatgpt-shell-chatgpt-prompt
             chatgpt-shell-send-region
             chatgpt-shell-send-and-review-region
             chatgpt-shell-prompt

             myai-with-custom-system
             myai-reset-system-prompt)
  :init

  ;; TODO move these to autoload.el without breaking header-line-format :eval
  (defun chatgpt-shell-header-function ()
    (let* ((left (propertize chatgpt-shell-model-version 'face 'bold))
           (right chatgpt-shell-system-prompt)
           (avail-width (- (window-width) (length left)))
           (right (if (>= (+ 5 (length right)) avail-width)
                      (if (fboundp 'string-truncate-left)
                          (string-truncate-left right (- avail-width 5))
                        (if (fboundp 's-truncate)
                            (s-truncate (- avail-width 5) right)
                          right))
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

  (setq chatgpt-shell-system-prompt (alist-get 'default myai-system-prompts))
  (setq chatgpt-shell-model-version (car myai-model-versions))

  (when (modulep! :lang org)
    (add-hook 'org-mode-hook (lambda ()
                               ()
                               (require 'ob-chatgpt-shell nil t)))))

(use-package! dall-e-shell
  :commands (dall-e-shell))
