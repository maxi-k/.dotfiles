;; -*- no-byte-compile: t; -*-
;;; tools/openai/packages.el

(package! gptai
  :recipe (:host github
           :repo "antonhibl/gptai"
           :files ("*.el")))
