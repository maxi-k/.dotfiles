;; -*- no-byte-compile: t; -*-
;;; tools/openai/packages.el

(package! gptel
  :recipe (:host github :repo "karthink/gptel"))

(package! chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell"))
