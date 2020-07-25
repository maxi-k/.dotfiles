;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Maximilian Kuschewski"
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Notes")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Don't open the doom dashboard at startup
;; Use the org notes file instead.
(setq initial-buffer-choice (expand-file-name +org-capture-notes-file org-directory))

;; Still new to doom, show which-key faster
(setq which-key-idle-delay 0.5)

;; Keep a scroll margin
(setq scroll-margin 7)

;; TODO find out why this doesn't work
;; Highlight lines > 100 in length
(setq whitespace-line-column 100
      whitespace-style '(face trailing lines-tail tabs))

;; Make duplicate buffer names more discernible
;; - post-forward: display the directory name after the buffer name
(setq uniquify-buffer-name-style 'post-forward)

;; Completely indent and untabify a buffer
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun visit-notes-buffer ()
  "Visit the initial buffer"
  (interactive)
  (find-file (expand-file-name +org-capture-notes-file org-directory)))

;; Import some important  keys from my own config
(map!
 ;; Global Keys
 (:when (featurep! :tools magit)
  (:prefix "C-c"
   :desc "Git"    "g"    #'magit-status))
 ;; Leader Keys
 :leader
 (:prefix "o"
  :desc "News"   "n"    #'elfeed
  :desc "Doom"   "d"    #'+doom-dashboard/open
  :desc "Org"    "o"    #'visit-notes-buffer)
 (:when (featurep! :editor evil)
  (:prefix "b"
   "v" #'evil-switch-to-windows-last-buffer
   "c" #'cleanup-buffer)))


;;;  TODO: Features to import from old config:
;;;
;; (config/provide-feature :movement
;;   "j" 'avy-goto-char
;;   "w" 'avy-goto-word-or-subword-1)

;; (config/global-keys
;;   "m" 'avy-goto-word-or-subword-1)

;; (global-set-key (kbd "s-i") 'imenu)
;; (config/provide-feature :movement "i" 'imenu)
;;
;; * :use-package browse-kill-ring
;; ** :defer t
;; ** :bind
;; Look through the kill ring and insert exactly what you want.
;; #+begin_src emacs-lisp
;; ("C-x C-y" . browse-kill-ring)
;;
;; golden-ratio-mode
