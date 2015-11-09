(use-package haskell-mode
  :mode ("\\.hs\\'" "\\.lhs\\'")

  :init
  ;; We need to establish both an interaction mode and an indentation mode.
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (setq haskell-process-type 'stack))
