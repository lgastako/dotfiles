(require 'haskell-mode)

;;'(haskell-mode-hook (quote (paredit-mode capitalized-words-mode turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indent turn-on-haskell-indentation turn-on-haskell-simple-indent)))

;; We need to establish both an interaction mode and an indentation mode.
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Since we are sane, we use cabal sandboxes, so we need this:

;; I think this is for haskell-interaction-mode
;;(setq haskell-process-type 'cabal-repl)
;; This is for the other
;;(setq haskell-program-name "cabal repl")
