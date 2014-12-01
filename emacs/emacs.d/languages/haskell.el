;;(add-to-list 'load-path "~/dotfiles/emacs/emacs.d/plugins/structured-haskell-mode")
;;(require 'shm)

;; Do not add a hook for haskell-indentation-modes.  SHM prevails.
;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; (eval-after-load 'haskell
;;   '(define-key ))

;; (add-hook 'haskell-mode
;;           (lambda () (local-set-key (kbd "C-c") #'haskell-compile)))

;; breaks on startup
;;(speedbar-add-supported-extension ".hs")

;;(setq shm-program-name "~/local/bin/structured-haskell-mode")

;; (eval-after-load "haskell-mode"
;;   '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

;; (eval-after-load "haskell-cabal"
;;   '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;(setq haskell-process-type 'cabal-repl)

