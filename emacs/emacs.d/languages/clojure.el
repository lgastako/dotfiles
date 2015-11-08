(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljx$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.boot$" . clojure-mode))

;; Clojure Files
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;; In the REPL
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(setq cider-lein-command "/Users/john/dotfiles/bin/lein")
(setq cider-boot-command "/Users/john/dotfiles/bin/boot")

(setq cider-overlays-use-font-lock t)

;; Doesn't work anyway... try with use-package maybe?
;; Is there a way to just set it on instead of toggling it assuming it's off?
;; (cider-repl-toggle-pretty-printing)
