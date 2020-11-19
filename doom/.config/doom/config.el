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

(setq doom-theme (if window-system 'doom-one 'bleak))

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
;; (setq initial-buffer-choice (expand-file-name +org-capture-notes-file org-directory))

;; Still new to doom, show which-key faster
(setq which-key-idle-delay 1.5)
(after! company
  (setq company-idle-delay 1.0))

;; Keep a scroll margin
(setq scroll-margin 7)

;; TODO find out why this doesn't work
;; Highlight lines > 100 in length
(setq whitespace-line-column 100
      whitespace-style '(face trailing lines-tail tabs))

;; Make duplicate buffer names more discernible
;; - post-forward: display the directory name after the buffer name
(setq uniquify-buffer-name-style 'post-forward)

(defun visit-notes-buffer ()
  "Visit the main org mode notes buffer."
  (interactive)
  (find-file (expand-file-name +org-capture-notes-file org-directory)))

(defun toggle-window-split ()
  "Toggle the window split between horizontal and vertial."
  (interactive)
  (when (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun open-external-terminal ()
  "Open an external terminal in the current directory asynchronously."
  (interactive)
  (let ((term (or (getenv "TERMINAL") "alacritty")))
    (async-shell-command term nil nil)))

;; Import some important  keys from my own config
(map!
 ;; Global Keys
 "M-O" #'toggle-window-split

 (:when (featurep! :tools magit)
  :desc "Git"    "C-c g"    #'magit-status)
 (:when (featurep! :emacs browse-kill-ring)
  "C-x C-y"  #'browse-kill-ring)

 ;; Leader Keys
 :leader

 ;; buffer commands
 (:prefix "b"
  (:when (featurep! :editor evil))
  "v" #'evil-switch-to-windows-last-buffer)

 ;; "open commands / apps"
 (:prefix "o"
  :desc "Term Here"    "h"    #'terminal-here-launch
  :desc "Term Project" "p"    #'terminal-here-project-launch
  (:when (featurep! :app rss)
   :desc "News"        "n"    #'elfeed)
  (:when (featurep! :ui doom-dashboard)
   :desc "Doom"        "d"    #'+doom-dashboard/open)
  (:when (featurep! :lang org)
   :desc "Org"         "o"    #'visit-notes-buffer))

 ;; search commands
 (:prefix "s"
  (:when (featurep! :emacs browse-kill-ring)
   "c" #'browse-kill-ring))

 ;; window commands
 (:prefix "w"
  (:when (featurep! :ui golden-ratio)
   "g" #'+golden-ratio-toggle))

 ;; Direct Keybindings for jumping with avy
 :desc "Jump Char" "J" #'avy-goto-char
 :desc "Jump Word" "j" #'avy-goto-word-or-subword-1)


;; Mode Variables
(after! rustic
  (when (featurep! :tools lsp)
    (setq rustic-lsp-server 'rust-analyzer)))

(after! org
  (setq org-startup-folded 't))

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(after! ruby
  (when (featurep! :tools lsp)
    (setq lsp-solargraph-use-bundler 't)))

(let ((notes-directory "~/Documents/Notes/roam"))
  (setq deft-directory notes-directory)
  (setq org-roam-directory notes-directory))

(when (featurep! :lang org +roam)
  (map!
   :leader
   (:prefix "n"
    (:prefix "r"
      "q" #'org-roam-buffer-toggle-display
      "t" #'org-roam-dailies-find-today))))
