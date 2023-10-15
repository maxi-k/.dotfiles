;;; lang/bison/config.el -*- lexical-binding: t; -*-

(use-package! bison-mode
  :commands (bison-mode yacc-mode)
  :mode ("\\.ycc\\'" "\\.ypp\\'"))
