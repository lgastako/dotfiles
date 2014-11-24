(add-to-load-path-list "/usr/local/Cellar/go/1.0.3/misc/emacs")
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")

(require 'go-mode-load)
;;(require 'go-flymake)

(setq gofmt-command "goimports")

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook #'enable-paredit-mode)

(setenv "GOPATH" "~/go")

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/local/go/bin")))
(setq exec-path (append exec-path '("~/go/bin")))

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

