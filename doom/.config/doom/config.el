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
(setq doom-font (font-spec :family "JetBrains Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "Inter")
      doom-symbol-font (font-spec :family "Noto Color Emoji"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-solarized-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'.

(setq my/notes-directory "~/Documents/Notes/")
(defvar my/notes-dashboard-file
  (concat my/notes-directory "home.org")
  "A 'default' file to use when opening notes.")

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
  (setq company-idle-delay 1.0
        company-tooltip-idle-delay 0.325))

;; Keep a scroll margin
(setq scroll-margin 7)

;; TODO find out why this doesn't work
;; Highlight lines > 100 in length
(setq whitespace-line-column 100
      whitespace-style '(face trailing lines-tail tabs))

;; Make duplicate buffer names more discernible
;; - post-forward: display the directory name after the buffer name
(setq uniquify-buffer-name-style 'post-forward)

;; (setq enable-local-variables t)

;;
;; Some utility functions
;;

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

(defun my/insert-git-commit-hash-at-point ()
  "Insert a (short, 7 digit) git commit hash at the current point."
  (interactive)
  (shell-command "git rev-parse --verify HEAD | cut -c -7" 1)
  ;; TODO better way to do this after insert?
  ;; other version of `shell-command` that auto-moves cursor?
  (right-char 7))

(defun open-external-terminal ()
  "Open an external terminal in the current directory asynchronously."
  (interactive)
  (let ((term (or (getenv "TERMINAL") "alacritty")))
    (async-shell-command term nil nil)))

(when (modulep! :ui popup)
  (defun +popup/toggle-for-buffer ()
    "Raise or degrade the current buffer to/from a popup window
depending on the current stat."
    (interactive)
    (call-interactively (if (+popup-buffer-p)
                            #'+popup/raise
                          #'+popup/buffer))))

(when (and (modulep! :completion ivy)
           (modulep! :editor evil))
  (after! ivy (add-hook! ivy-occur (turn-off-evil-mode))))

(defun pinentry-emacs (desc prompt ok error)
  "Function for using emacs as a pinentry service."
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(defun my/capitalize-words-in-string (str)
  "Capitalize the words in the given string."
  (string-join
   (mapcar (lambda (s) (and s (not (string-blank-p s)) (concat (capitalize (substring s nil 1)) (substring s 1))))
           (split-string str))
   " "))

;;
;; Lisp language family setup
;;

(defun my/on-lisp-mode ()
  "Call on entering a lisp mode."
  (interactive)
  (paredit-mode)
  (rainbow-delimiters-mode)
  (smartparens-mode -1) ;; also generates closing single quotes
  (when (and (modulep! :editor evil)
             (modulep! :editor evil-cleverparens))
    (evil-cleverparens-mode 1)))

(add-hook! 'lisp-mode-hook (my/on-lisp-mode))
(add-hook! emacs-lisp-mode (my/on-lisp-mode))
(when (modulep! :lang lfe)
  (add-hook! lfe-mode (my/on-lisp-mode)))
(when (modulep! :lang clojure)
  (add-hook! clojure-mode (my/on-lisp-mode)))

;; Language Server Protocol setup
(when (modulep! :tools lsp)
  (setq lsp-lens-auto-enable nil)  ;; lens seems to deteriorate performance (tested in c++ mode), disable it for now
  (after! rustic (setq rustic-lsp-server 'rust-analyzer))
  (after! ruby (setq lsp-solargraph-use-bundler 't))
  (after! lsp
    (map!
     :leader
     :prefix "b"
     ("=" #'lsp-format-buffer))))

(when (modulep! :lang rust)
  (setq rust-format-on-save t))

;; Deft for note searching in the notes/roam directory
(when (modulep! :ui deft)
  (setq deft-directory my/notes-directory
        deft-recursive 't))

(when (modulep! :lang org)
  (after! org-fold-core ;; fixes evil-mode search issue in folded org sections, see: https://github.com/doomemacs/doomemacs/issues/6478
    (setq org-fold-core-style 'overlays))
  (after! ox-latex
    (setq org-latex-inputenc-alist '(("utf8" . "utf8x"))))
  (map!
   (:map org-mode-map
    :localleader
    ("L" #'org-cycle-list-bullet)))
  (after! org
    ;; used by org capture; default doom popup rule (:slot -1 :vslot -2 :ttl 0 :size 0.25)
    ;; seems to break this, causing the select buffer content to be inserted into the current buffer
    (set-popup-rule! "\\*Org Select\\*" :size 0.42 :quit nil :select t :autosave 'ignore)))

(when (modulep! :lang org +roam2)
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry
           "* %?"
           :target (file+head+olp "%<%Y-w%W>.org" "#+title: %<%Y-w%W>\n" ("%<%A, %Y-%m-%d>")))))

  (after! org
    (setq org-tags-exclude-from-inheritance (delete-dups (cons "list" org-tags-exclude-from-inheritance)))
    ;; adapted from https://jethrokuan.github.io/org-roam-guide/
    (setq org-roam-capture-templates
          `(("d" "default" plain "%?"
             :if-new (file+head "resources/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
             :unnarrowed t)
            ("e" "event" plain "%?"
             :if-new (file+head "events/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :event:\n")
             :immediate-finish t
             :unnarrowed t)
            ("r" "reference" plain "%?"
             :if-new (file+head "references/${slug}.org" "#+title: ${title}\n#+filetags: :paper:reference:\n")
             :immediate-finish t
             :unnarrowed t)
            ("i" "idea" plain "%?"
             :if-new (file+head "resources/${slug}.org" "#+title: ${title}\n#+filetags: :idea:\n")
             :immediate-finish t
             :unnarrowed t)
            ("p" "project" plain "%?"
             :if-new (file+head "projects/${slug}.org" "#+title: ${title}\n#+filetags: :project:\n")
             :immediate-finish t
             :unnarrowed t)
            ("a" "area" plain "%?"
             :if-new (file+head "projects/${slug}.org" "#+title: ${title}\n#+filetags: :area:\n")
             :immediate-finish t
             :unnarrowed t)
            ("x" "experiment" plain "%?"
             :if-new (file+head "experiments/${slug}.org" "#+title: ${title}\n#+filetags: :experiment:research:\n")
             :immediate-finish t
             :unnarrowed t)
            ("P" "person" plain "* %(my/capitalize-words-in-string \"${title}\")\n:PROPERTIES:\n:ID:  %(org-id-uuid)\n:END:\n %?"
             :target (node "persons")
             :prepend nil
             :immediate-finish t)
            )))

  (setq org-roam-node-display-template
        (format "%s ${doom-hierarchy:*} %s %s"
                (propertize "${doom-type:15}" 'face 'font-lock-comment-face)
                ;; right align doom tags; simplified version of
                ;; https://github.com/org-roam/org-roam/issues/1775#issue-971157225
                (propertize " " 'display `(space :align-to (- right 30)))
                (propertize "${doom-tags:30}" 'face 'org-tag))))

;; Make macOS title bar transparent
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(defun my/grep-page-break ()
  "Grep the current buffer for page breaks ()."
  (interactive)
  (counsel-grep-or-swiper ""))
;;
;; Global Keybindings
;;

;; Import some important keys from my own config
(map!
 ;; Global Keys
 "M-O" #'toggle-window-split
 "C-;" #'avy-goto-word-or-subword-1
 "C-," #'er/expand-region

 (:when (modulep! :tools magit)
  "C-c g"    #'magit-status)

 (:when (modulep! :emacs browse-kill-ring)
  "C-x C-y"  #'browse-kill-ring)

 ;; Leader Keys
 :leader
 ;; insert commands
 (:prefix "i"
  (:when (modulep! :editor evil)
   "i" #'evil-insert-digraph
   "d" #'evil-ex-show-digraphs
   "c" #'my/insert-git-commit-hash-at-point))

 ;; buffer commands
 (:prefix "b"
  (:when (modulep! :editor evil)
   "v" #'evil-switch-to-windows-last-buffer)
  (:when (modulep! :ui popup)
   "~" #'+popup/toggle-for-buffer))

 ;; additional note taking commands
 (:prefix "n"
          (:when (modulep! :lang org +roam2)
            "g" #'org-open-at-point-global
            (:when (modulep! :ui popup)
              ;; leader-x is the normal scratch buffer
              "x" #'my/roam-daily-as-popup)
            (:prefix "r"
                     (:when (modulep! :ui org-dashboard-roam)
                       "h" (lambda () (interactive) (find-file my/notes-dashboard-file)))))
  )

 ;; "open commands / apps"
 (:prefix "o"
  :desc "Term Here"    "h"    #'terminal-here-launch
  :desc "Term Project" "p"    #'terminal-here-project-launch
  :desc "Calc"         "c"    #'calc
  :desc "Calc Region"  "C"    #'calc-grab-region
  (:when (modulep! :app rss)
   :desc "News"        "n"    #'elfeed)
  (:when (modulep! :ui doom-dashboard)
   :desc "Doom"        "d"    #'+doom-dashboard/open)
  (:when (modulep! :lang org)
   :desc "Org"         "o"    #'visit-notes-buffer))

 ;; search commands
 (:prefix "s"
  "I" #'imenu
  (:when (modulep! :emacs browse-kill-ring)
   "c" #'browse-kill-ring)
  (:when (modulep! :completion ivy)
    "z" #'my/grep-page-break)
  (:when (modulep! :tools translation)
    ;; replaces dooms default 'thesaurus' keybinding
    "T" #'+translate/online))

 ;; window commands
 (:prefix "w"
  "'" #'toggle-window-split
  (:when (modulep! :ui golden-ratio)
   "g" #'+golden-ratio-toggle))



 ;; Direct Keybindings for jumping with avy
 :desc "Jump Char" "J" #'avy-goto-char
 :desc "Jump Word" "j" #'avy-goto-word-or-subword-1)

;;
;; Very package- / mode-specific keybindings
;;

(after! ess
  (map!
   (:map ess-mode-map
    :localleader
    ("e" #'ess-eval-region-or-function-or-paragraph))))

(after! elfeed
  (map!
   (:map elfeed-search-mode-map
    :localleader
    "u" #'elfeed-update
    "r" #'elfeed-search-update)))

(after! paredit
  (map!
   :map paredit-mode-map
   [(meta shift backspace)] #'paredit-splice-sexp-killing-backward))

(when (modulep! :editor lispy)
  (map!
   :map lispy-mode-map
   :localleader
   "s" #'lispy-splice-sexp-killing-backward
   "S" #'lispy-splice-sexp-killing-forward))

;; (when (modulep! :checkers spell)
;;   (add-hook! spell-fu-mode
;;     (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "german-new8"))
;;     (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "deutsch8"))
;;   ))

(defvar *scratchpad-theme* 'doom-opera-light)
(defvar *scratchpad-action*
  :daily
  "The action to take when running `my/scratchpad-profile':
:scratch
        Opens the *scratch* buffer
:dashboard
        Opens `my/notes-dashboard-file
:daily
        If `org-roam' is loaded, opens the daily note
:mail
        If `notmuch' is loaded, opens notmuch ")

(defun my/scratchpad-profile (&optional action)
  "Run commands that make emacs usable as a 'scratchpad'"
  (interactive)
  (message "loading scratchpad profile")
  (setq doom-theme *scratchpad-theme*)
  (if doom-init-time
      (load-theme *scratchpad-theme*)
    (add-hook 'window-setup-hook (lambda () (load-theme *scratchpad-theme*))))
  (let ((x (or action *scratchpad-action*)))
    (cond
     ((eq x :scratch) (switch-to-buffer "*scratch*"))
     ((eq x :dashboard) (find-file my/notes-dashboard-file))
     ((eq x :daily) (when (modulep! :lang org +roam2)
                     (org-roam-dailies-goto-today)))
     ((eq x :notmuch) (when (modulep! :email notmuch)
                       (=notmuch)))
     (t (message "Unknown *scratchpad-action* %s, doing nothing" x)))))

(when (modulep! :email notmuch)

  (defvar my/notmuch-todo-query "tag:unread or tag:flagged or (tag:inbox and not (tag:trash or tag:deleted))")
  (defun my/notmuch-inbox ()
    (interactive)
    (notmuch-search my/notmuch-todo-query))

  (setq +notmuch-home-function #'my/notmuch-inbox ;; directly go to the inbox instead of opening notmuch-hello
        +notmuch-sync-backend 'mbsync
        notmuch-show-logo nil)

    ;; sending mail
    (setq send-mail-function 'message-send-mail-with-sendmail
          sendmail-program "/usr/bin/msmtp"
          ;; sendmail should figure out the 'from' address from the envelope
          ;; instead of generating a weird ID
          mail-specify-envelope-from t
          mail-envelope-from ""
          ;;message-sendmail-extra-arguments '("--read-envelope-from")
          mail-sendmail-extra-arguments nil
          message-sendmail-envelope-from 'obey-mail-envelope-from
          mail-host-address "maxi.fyi")



    (after! notmuch

      (setq notmuch-archive-tags  '("-inbox" "-unread" "+archive"))

      (setq notmuch-tagging-keys
            `((,(kbd "a") notmuch-archive-tags "Archive")
              (,(kbd "r") notmuch-show-mark-read-tags "Mark read")
              (,(kbd "u") ("+inbox +unread") "Mark unread")
              (,(kbd "f") ("+flagged") "Flag")
              (,(kbd "A") +notmuch-spam-tags "Mark as spam")
              (,(kbd "d") ("+deleted" "-inbox") "Delete")))

      (setq notmuch-saved-searches
            `((:name "todo" :query ,my/notmuch-todo-query :key "t")
              (:name "inbox" :query "tag:inbox not tag:trash" :key "i")
              (:name "unread" :query "tag:unread" :key "u")
              (:name "flagged" :query "tag:flagged" :key "f")
              (:name "sent" :query "tag:sent or tag:replied or path:/Sent.*/" :key "s")
              (:name "drafts" :query "tag:draft" :key "d")
              (:name "archive" :query "tag:archive" :key "A")
              (:name "all mail" :query "*" :key "a")))


      (map! :map (notmuch-search-mode-map notmuch-tree-mode-map)
            "T" #'notmuch-tag-jump
            :localleader
            "t" #'notmuch-tag-jump)

      (advice-add '+notmuch/compose
                  :override (lambda ()
                              (notmuch-mua-mail
                               nil
                               nil
                               (list (cons 'From
                                           (format "%s <%s>" user-full-name
                                                   (completing-read "From: " (notmuch-user-emails))))))))))

(when (modulep! :tools openai)
  (map! :leader
        (:prefix ("a" . "AI")
                 "." #'ai-buffer
                 "b" #'ai-buffer
                 "s" #'ai-send
                 "a" #'ai-add-context
                 "f" #'ai-add-file
                 "o" #'ai-org-set-topic
                 "O" #'ai-org-set-properties)))

(ignore
 '(when (modulep! :tools lsp)
   (defun lsp-booster--advice-json-parse (old-fn &rest args)
     "Try to parse bytecode instead of json."
     (or
      (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))
   (advice-add (if (progn (require 'json)
                          (fboundp 'json-parse-buffer))
                   'json-parse-buffer
                 'json-read)
               :around
               #'lsp-booster--advice-json-parse)

   (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
     "Prepend emacs-lsp-booster command to lsp CMD."
     (let ((orig-result (funcall old-fn cmd test?)))
       (if (and (not test?)                             ;; for check lsp-server-present?
                (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                lsp-use-plists
                (not (functionp 'json-rpc-connection))  ;; native json-rpc
                (executable-find "emacs-lsp-booster"))
           (progn
             (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
               (setcar orig-result command-from-exec-path))
             (message "Using emacs-lsp-booster for %s!" orig-result)
             (cons "emacs-lsp-booster" orig-result))
         orig-result)))
   (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

;; TODO is there a better way to do this built into doom?
;; private git-ignored configuration variables
(when (file-exists-p! "config.local.el" doom-user-dir)
  (load! "config.local.el" doom-user-dir))
