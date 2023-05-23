;; -*- no-byte-compile: t; -*-
;;; editor/copilot/packages.el

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
