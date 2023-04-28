;; -*- no-byte-compile: t; -*-
;;; tools/openai/packages.el

(package! gptel
  :recipe (:host github :repo "karthink/gptel"))

(package! shell-maker
  :recipe (:host github :repo "xenodium/chatgpt-shell"))

(package! chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell"))
