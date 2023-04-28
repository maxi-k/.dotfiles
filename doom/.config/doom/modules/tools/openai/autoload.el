;;; tools/openai/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun myai--get-system-prompt-options ()
  (delete-dups (append myai-system-prompts myai-prompt-history nil)))

;;;###autoload
(defun myai--read-user-system-prompt-options (&rest config)
  (let* ((options (or (plist-get config :options) (myai--get-system-prompt-options)))
        (prompt (or (plist-get config :prompt) "System Role: "))
        (result (completing-read prompt options))
        (known (alist-get (intern result) myai-system-prompts)))
    (unless known (add-to-list 'myai-prompt-history result))
    (or known result)))

;;;###autoload
(defun myai-with-custom-system (set-globally)
  (interactive "P")
  (let ((system-prompt (myai--read-user-system-prompt-options)))
    (when set-globally
      (setq chatgpt-shell-system-prompt system-prompt))

    (let ((chatgpt-shell-system-prompt system-prompt))
      (call-interactively #'chatgpt-shell-prompt))))

;;;###autoload
(defun myai-reset-system-prompt (use-default)
  (interactive "P")
  (let ((new-prompt (if use-default
                        (alist-get 'default myai-system-prompts)
                        (myai--read-user-system-prompt-options :prompt "New Global System Role: "))))
    (setq chatgpt-shell-system-prompt new-prompt)))

;;;###autoload
(defun myai--read-user-model-prompt (&rest config)
  (completing-read (or (plist-get config :prompt) "Model: ")
                   (or (plist-get config :options) myai-model-versions)))

;;;###autoload
(defun myai-select-model (use-default)
  (interactive "P")
  (let ((new-val (if use-default
                        myai-default-model
                   (myai--read-user-model-prompt))))
    (setq chatgpt-shell-model-version new-val)
    (setq-default chatgpt-shell-model-version new-val)))
