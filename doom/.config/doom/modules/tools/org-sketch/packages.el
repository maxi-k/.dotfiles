;; -*- no-byte-compile: t; -*-
;;; tools/org-sketch/packages.el

(package! org-xournalpp
  :recipe (:host gitlab
           :repo "vherrmann/org-xournalpp"
           :files ("resources" "*.el")))
