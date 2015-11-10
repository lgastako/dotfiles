(use-package clojure-mode
  :mode ("\\.clj\\'" "\\.cljs\\'" "\\.cljc\\'")
  :init

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

  (setq cider-lein-command "/Users/john/dotfiles/bin/lein"
        cider-boot-command "/Users/john/dotfiles/bin/boot")

  (setq cider-overlays-use-font-lock t)

  ;; (setq cider-auto-select-error-buffer t)

  :config
  (rename-modeline "clojure-mode" clojure-mode "λ"))

(use-package clj-refactor
  :init   (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  :config (cljr-add-keybindings-with-prefix "C-!"))
