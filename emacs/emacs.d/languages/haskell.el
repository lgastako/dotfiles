(add-to-list 'load-path "~/dotfiles/emacs/emacs.d/plugins/structured-haskell-mode")
(require 'shm)

;; Do not add a hook for haskell-indentation-modes.  SHM prevails.
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; (eval-after-load 'haskell
;;   '(define-key ))

;; (add-hook 'haskell-mode
;;           (lambda () (local-set-key (kbd "C-c") #'haskell-compile)))

(speedbar-add-supported-extension ".hs")
(setq shm-program-name "~/local/bin/structured-haskell-mode")
