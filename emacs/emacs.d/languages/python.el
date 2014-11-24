
(add-hook 'python-mode-hook 'fci-mode)

(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

;; Ropemacs fucks things up, so instead we'll load specific things..

;;(load-file "~/dotfiles/emacs/emacs.d/emacs-for-python/epy-init.el")

;; tell where to load the various files
;; (add-to-list 'load-path "~/dotfiles/emacs/emacs.d/emacs-for-python/")

;; (require 'epy-setup)      ;; It will setup other loads, it is required!
;; (require 'epy-python)     ;; If you want the python facilities [optional]
;; (require 'epy-completion) ;; If you want the autocompletion settings [optional]
;; (require 'epy-editing)    ;; For configurations related to editing [optional]
;; (require 'epy-bindings)   ;; For my suggested keybindings [optional]
;; (require 'epy-nose)       ;; For nose integration

;; (epy-setup-ipython)

