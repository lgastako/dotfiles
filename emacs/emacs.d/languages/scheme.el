(require 'quack)
(require 'geiser)

(setq geiser-active-implementations '(racket))
(setq geiser-racket-binary "/Users/john/local/bin/racket")

(add-hook 'scheme-mode-hook #'enable-paredit-mode)
