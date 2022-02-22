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

(setq doom-theme (if window-system 'doom-nord 'bleak))

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
  (setq company-idle-delay 1.0)
  (setq company-tooltip-idle-delay 0.325))

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

(defun +popup/toggle-for-buffer ()
  "Raise or degrade the current buffer to/from a popup window
depending on the current stat."
  (interactive)
  (call-interactively (if (+popup-buffer-p)
                          #'+popup/raise
                        #'+popup/buffer)))

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
 (:prefix "i"
  (:when (featurep! :editor evil)
   "i" #'evil-insert-digraph
   "d" #'evil-ex-show-digraphs))

 ;; buffer commands
 (:prefix "b"
  (:when (featurep! :editor evil)
   "v" #'evil-switch-to-windows-last-buffer)
  (:when (featurep! :ui popup)
   "~" #'+popup/toggle-for-buffer))

 ;; "open commands / apps"
 (:prefix "o"
  :desc "Term Here"    "h"    #'terminal-here-launch
  :desc "Term Project" "p"    #'terminal-here-project-launch
  :desc "Calc"         "c"    #'calc
  :desc "Calc Region"  "C"    #'calc-grab-region
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
  "'" #'toggle-window-split
  (:when (featurep! :ui golden-ratio)
   "g" #'+golden-ratio-toggle))

 ;; Direct Keybindings for jumping with avy
 :desc "Jump Char" "J" #'avy-goto-char
 :desc "Jump Word" "j" #'avy-goto-word-or-subword-1)


;; Mode Variables for the language server protocol
(when (featurep! :tools lsp)
  (setq lsp-lens-auto-enable nil)  ;; lens seems to deteriorate performance (tested in c++ mode), disable it for now
  (after! rustic (setq rustic-lsp-server 'rust-analyzer))
  (after! ruby (setq lsp-solargraph-use-bundler 't)))

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(when (featurep! :lang org)
  (defun visit-notes-buffer ()
    "Visit the main org mode notes buffer."
    (interactive)
    (find-file (expand-file-name +org-capture-notes-file org-directory)))

  (defun org-directory-files (&optional root)
    "List org files in the given directory.
If no directory is given,assume the current one (`default-directory`)."
    (let ((directory (or root default-directory)))
      (directory-files directory nil ".org")))

  (defun org-recursive-directory-files (&optional root)
    "List org files in the given directory recursively.
If no directory is given, assume the current one (`default-directory`)."
    (let ((directory (or root default-directory)))
      (directory-files-recursively directory (rx ".org" eos))))

  ;; use "TODO" instead of "[ ]" for new todo entries as used by orgzly
  (after! org
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-startup-folded 't
          org-duration-format (quote h:mm)
          org-capture-templates
          (append '(("t" "Personal todo" entry
                     (file+headline +org-capture-todo-file "Inbox")
                     "* TODO %?\n" :prepend t)
                    ("f" "File todo" entry
                     (file+headline +org-capture-todo-file "Inbox")
                     "* TODO %?\n%i\n%a" :prepend t))
                  (cdr org-capture-templates))
          org-latex-pdf-process '("latexmk -lualatex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
          org-latex-listings 'minted))


  (defun my/org-insert-timestamped-list-item (&optional other-time active)
    "Insert a (inactive) timestamped list item with `+org/insert-item-below`.
Use the prefix argument to specify the time.
Use two prefix arguments to insert an active timestamp instead."
    (interactive "P")
    (+org/insert-item-below 1)
    (org-time-stamp (if other-time 1 '(16)) (if active nil 'inactive))
    (insert-char 32)) ;; space afterwards

  (defun my/org-insert-timestamped-list-item-prompt (&optional active)
    "Insert a (inactive) timestamped list item with `+org/insert-item-below`, prompting for the time.
Use a prefix argument to insert an active timestamp instead."
    (interactive "P")
    (my/org-insert-timestamped-list-item 't active))

  (map!
   (:map org-mode-map :localleader
    (:prefix "d"
     "l" #'my/org-insert-timestamped-list-item
     "i" #'my/org-insert-timestamped-list-item-prompt)))
  )

(let ((notes-directory "~/Documents/Notes/roam"))
  (setq deft-directory notes-directory)
  (setq deft-recursive 't)
  (setq org-roam-directory notes-directory))

(when (featurep! :lang org +roam2)
  (after! org-roam
    ;; Don't convert to file links, keep heading-based roam: links instead
    ;; (setq org-roam-link-auto-replace nil)

    ;; TODO version that replaces every link with
    ;; org-next-link
    (defun my/org-roam-replace-link (&optional arg)
      "Replace the link at point with a new roam-style link.
  Return 't if it was replaced, nil otherwise"
      (interactive "P")
      (save-excursion
        (let ((context (org-element-context)))
          (pcase (org-element-lineage context '(link) t)
            (`nil nil)
            (link
             (let* ((link-type (org-element-property :type link))
                    (link-content (cond ((string-equal "roam" link-type) nil)
                                        ((string-equal "fuzzy" link-type) (org-element-property :raw-link link))
                                        (t (let* ((begin (or (org-element-property :contents-begin link)
                                                             (org-element-property :begin link)))
                                                  (end (or (org-element-property :contents-end link)
                                                           (org-element-property :end link))))
                                             (when (and begin end)
                                               (string-trim (buffer-substring-no-properties begin end))))))))
               (when link-content
                 (save-match-data
                   (if (org-in-regexp org-link-bracket-re 1)
                       (progn
                         (replace-match "")
                         (insert (org-link-make-string (concat "roam:" link-content) link-content)))
                     (message "No link at point"))))
               (when arg
                 (org-roam-link-replace-at-point))))))))

    (defun my/walk-org-links (f)
      "Walk org mode links in a buffer, calling `f` at each point. Does not save the excursion."
      (pcase (org-next-link)
        ("No further link found" nil)
        (res
         (funcall f)
         (my/walk-org-links f))))

    (defun my/migrate-org-roam-buffer ()
      "Migrate the given buffer to org roam v2 if possible."
      (interactive)
      (save-excursion
        (my/walk-org-links #'my/org-roam-replace-link)
        (org-roam-link-replace-all)))

    (defmacro my/infile-no-traces (file &rest body)
      `(let ((no-traces|visited-p (get-file-buffer (expand-file-name ,file)))
             to-be-removed)
         (save-window-excursion
           (find-file ,file)
           (setq to-be-removed (current-buffer))
           ,@body)
         (unless no-traces|visited-p
           (kill-buffer to-be-removed))))

    (defun my/migrate-org-roam-file (file)
      (my/infile-no-traces file
                           (message "Migrating %s" file)
                           (my/migrate-org-roam-buffer)
                           (save-buffer)))

    (defun my/migrate-org-roam-directory (dir)
      (let ((files (directory-files dir  t "\\.org$")))
        (mapc #'my/migrate-org-roam-file files)))

    (defun my/org-roam-replace-and-follow ()
      "Replaces the link at point with a roam: style link, then follows it."
      (interactive)
      (my/org-roam-replace-link)
      (let ((org-roam-link-auto-replace nil))
        (org-open-at-point)))

    (map!
     (:map org-mode-map
      "M-]" #'my/org-roam-replace-link
      :localleader
      (:prefix "m"
       "l" #'my/org-roam-replace-and-follow
       "L" #'my/org-roam-replace-link))
     :leader
     (:prefix "n"
      (:prefix "r"
       "t" #'org-roam-dailies-today))))

  ;; Modify update mode to run in background
  (defun my/org-roam-on-save-autosync (&optional file-path)
    (when org-roam-db-update-on-save
      (let ((file (or file-path (buffer-file-name (buffer-base-buffer)))))
        (message "will run async with file %s" file)
        (make-thread
         (lambda ()
           (message "start async db update for file %s" file)
           (org-roam-db-update-file file)
           (message "done with async db update for file %s" file))))))

  ;; doesn't work yet:
  ;; (error "Attempt to accept output from process emacsql-sqlite locked to thread #<thread 0x55ac05b5dc20>")
  ;; (advice-add 'org-roam-db-autosync--try-update-on-save-h :override #'my/org-roam-on-save-autosync)

  (defun my/roam-update-agenda-files ()
    "Update the `org-agenda-files` variable by adding all org-roam files which contain todo-like strings."
    ;; (with-temp-buffer (org-agenda-mode) org-todo-keywords-for-agenda)
    (let* ((todo-strings '("TODO" "WAIT" "HOLD" "IDEA" "STRT" "PROJ" "\\[ \\]" "\\[?\\]" "\\[X\\]"))
           (args (mapcar (lambda (s) (concat "-e '" s "' ")) todo-strings))
           (search-dir org-roam-directory)
           (default-directory search-dir)
           (exec (executable-find "rg"))
           (cmd (apply #'concat "rg " "--fixed-strings " "--files-with-matches " args))
           (original-agenda org-agenda-files))
      (if exec
          (async-start
           `(lambda ()
              (require 'subr-x)
              (remq nil
                    (mapcar
                     (lambda (f) (when (and f (not (string-empty-p f))) (concat ,search-dir f)))
                     (split-string
                      (shell-command-to-string ,cmd) "\n"))))
           `(lambda (result)
              (if (boundp 'my/original-org-agenda-files)
                  ;; TODO duplicates as in ~/foo and /home/user/foo are still possible; how to fix best?
                  (setq org-agenda-files (delete-dups (append my/original-org-agenda-files result)))
                (setq org-agenda-files (delete-dups (append org-agenda-files result)))
                (setq my/original-org-agenda-files '(,@original-agenda)))))
        (message "Couldn't find ripgrep for smart roam<->agenda file initialization."))))


  (after! org-agenda ;; TODO is this delayed correctly?
    (my/roam-update-agenda-files))

  (after! roam
    (add-hook! after-save-hook #'my/roam-update-agenda-files))
  )

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

(defun my/on-lisp-mode ()
  "Call on entering a lisp mode."
  (interactive)
  (paredit-mode)
  (rainbow-delimiters-mode)
  (smartparens-mode -1) ;; also generates closing single quotes
  (when (and (featurep! :editor evil)
             (featurep! :editor evil-cleverparens))
    (evil-cleverparens-mode 1)))

(add-hook! 'lisp-mode-hook (my/on-lisp-mode))
(add-hook! emacs-lisp-mode (my/on-lisp-mode))
(when (featurep! :lang lfe)
  (add-hook! lfe-mode (my/on-lisp-mode)))
(when (featurep! :lang clojure)
  (add-hook! clojure-mode (my/on-lisp-mode)))

(when (featurep! :editor lispy)
  (map!
   :map lispy-mode-map
   :localleader
   "s" #'lispy-splice-sexp-killing-backward
   "S" #'lispy-splice-sexp-killing-forward))

;; Make macOS title bar transparent
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))
