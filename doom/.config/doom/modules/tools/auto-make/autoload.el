;;; tools/auto-make/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun auto-make-run-make (&rest args)
  (interactive)
  (if auto-make-make-async-p
      (let ((async-shell-command-buffer 'confirm-kill-process)
            (display-buffer-alist (cons '("\\*Async Shell Command\\*" (display-buffer-no-window)) display-buffer-alist))
            (default-directory (locate-dominating-file default-directory "Makefile")))
        (async-shell-command auto-make-make-command))
    (let ((display-buffer-alist (cons '("\\*Shell Command\\*" (display-buffer-no-window)) display-buffer-alist))
          (default-directory (locate-dominating-file default-directory "Makefile")))
      (shell-command auto-make-make-command))))
