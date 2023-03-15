;;; tools/openai/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun openai-send-user-prompt ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'gptel-send)))
