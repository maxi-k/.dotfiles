;;; editor/copilot/config.el -*- lexical-binding: t; -*-

;; set to a large idle delay so that it doesn't slow down editing by default
(defvar my/default-copilot-idle-delay 5 "The default copilot idle delay for low-latency editing.")
(defvar my/vibe-mode-copilot-idle-delay 0 "Idle delay for fast copilot suggestions at the cost of jankier emacs.")

;; could also disable 'autocompletion (w/o calling copilot-complete) explicitly like this
;; (setq copilot-disable-predicates (list (lambda () t)))

;; accept completion from copilot and fallback to company
(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
  :init (setq copilot-idle-delay my/default-copilot-idle-delay)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("M-C" . 'copilot-complete)
              ("TAB" . 'copilot-accept-completion)
              ("C-<return>" . 'copilot-accept-completion)
              ("M-p" . 'copilot-previous-completion)
              ("M-n" . 'copilot-next-completion)
              ("C-c" . 'copilot-clear-overlay)
              ("C-g" . 'copilot-clear-overlay)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; define 'vibe-mode', which reduces copilot idle delay; allow local and global mode
(define-minor-mode vibe-mode
  "Vibe mode for fast copilot suggestions."
  :init-value nil
  :lighter " Vibe"
  :global nil
  (if vibe-mode
      (progn
        ;; (unless copilot-mode (copilot-mode 1))
        (setq-local copilot-idle-delay my/vibe-mode-copilot-idle-delay))
    (progn
      ;; (when copilot-mode (copilot-mode -1))
      (setq-local copilot-idle-delay my/default-copilot-idle-delay))))

;; convenience functions
(defun turn-on-vibe-mode () (interactive) (vibe-mode 1))
(defun turn-off-vibe-mode () (interactive) (vibe-mode -1))

;; allow globalized version
(define-globalized-minor-mode global-vibe-mode vibe-mode
  turn-on-vibe-mode
  :init-value nil
  :global t
  :group 'copilot)
