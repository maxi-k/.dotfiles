;;; tools/openai/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun openai-with-custom-system (set-globally)
  (interactive "P")
  (require 'gptel)
  (let* ((prompt-options (seq-union gptel--system-message-alist openai-prompt-history))
         (system-prompt (completing-read "System Role: " prompt-options)))
    (when set-globally
      (setq gptel--system-message system-prompt))
    (add-to-list 'openai-prompt-history system-prompt)
    (let ((gptel--system-message system-prompt))
      (call-interactively #'gptel-send))))

;;;###autoload
(defun openai-reset-system-prompt (prompt-user)
  (interactive "P")
  (let ((new-prompt (if prompt-user
                        (completing-read "New System Role: "
                                         (seq-union gptel--system-message-alist openai-prompt-history))
                      (alist-get 'default gptel--system-message-alist)
                        )))
    (setq gptel--system-message new-prompt)))
