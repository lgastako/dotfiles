(add-to-load-path-list "/usr/local/Cellar/go/1.0.3/misc/emacs")
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")

;; (use-package go-mode-load
;;   :init
;;   (setenv "GOPATH" "~/go")
;;   ;; These next two should probably be moved out to the top level, no?
;;   (setq exec-path (append exec-path '("/usr/local/bin")))
;;   (setq exec-path (append exec-path '("/usr/local/go/bin")))
;;   (setq exec-path (append exec-path '("~/go/bin")))
;;   :config
;;   ;; (use-package go-flymake
;;   ;;   :init
;;   ;;   (setq gofmt-command "goimports")
;;   ;;   :config
;;   ;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;   ;;   (add-hook 'go-mode-hook #'enable-paredit-mode))
;;   (use-package go-eldoc
;;     :config
;;     (add-hook 'go-mode-hook 'go-eldoc-setup)))
