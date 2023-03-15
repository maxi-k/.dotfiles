;;; tools/openai/config.el -*- lexical-binding: t; -*-

;; https://github.com/antonhibl/gptai/
(use-package! gptai
  :defer t
  :commands (gptai-send-query
             gptai-send-query-region
             gptai-send-query-buffer
             gptai-spellcheck-region
             gptai-elaborate-on-region
             gptai-code-query
             gptai-code-query-region
             gptai-explain-code-region
             gptai-fix-code-region
             gptai-document-code-region
             gptai-optimize-code-region
             gptai-improve-code-region
             gptai-send-image-query
             gptai-list-models
             ))
