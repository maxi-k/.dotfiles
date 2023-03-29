;;; tools/openai/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun openai--get-system-prompt-options ()
  (delete-dups (append gptel-directives openai-prompt-history nil)))

;;;###autoload
(defun openai--read-user-system-prompt-options (&rest config)
  (let ((options (or (plist-get config :options) (openai--get-system-prompt-options)))
        (prompt (or (plist-get config :prompt) "System Role:")))
    (completing-read prompt options)))

;;;###autoload
(defun openai-with-custom-system (set-globally)
  (interactive "P")
  (require 'gptel)
  (let ((system-prompt (openai--read-user-system-prompt-options)))
    (when set-globally
      (setq gptel--system-message system-prompt))
    (add-to-list 'openai-prompt-history system-prompt)
    (let ((gptel--system-message system-prompt))
      (call-interactively #'gptel-send))))

;;;###autoload
(defun openai-reset-system-prompt (use-default)
  (interactive "P")
  (require 'gptel)
  (let ((new-prompt (if use-default
                        (alist-get 'default gptel-directives)
                        (openai--read-user-system-prompt-options :prompt "New Global System Role:"))))
    (setq gptel--system-message new-prompt)))

;;;###autoload
(defun openai--read-user-model-prompt (&rest config)
  (completing-read (or (plist-get config :prompt) "Model: ")
                   (or (plist-get config :options) openai-model-list)))

;;;###autoload
(defun openai-select-model (use-default)
  (interactive "P")
  (setq gptel-model (if use-default
                        openai-default-model
                      (openai--read-user-model-prompt))))
