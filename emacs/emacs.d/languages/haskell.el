(require 'haskell-mode)

;; We need to establish both an interaction mode and an indentation mode.
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; Now, how do we make sandboxes work...
(setq haskell-process-type 'stack)
