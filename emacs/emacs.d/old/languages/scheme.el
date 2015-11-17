;; (use-package quack)
;; (use-package geiser
;;   :mode "\\.scm\\'"
;;   :config
;;   (setq geiser-active-implementations '(racket))
;;   (setq geiser-racket-binary "/Users/john/local/bin/racket"))

(use-package racket-mode
  :defer t
  :config
  (add-hook 'racket-mode-hook
            '(lambda ()
               (define-key racket-mode-map (kbd "C-c C-l") 'racket-run)
               (define-key racket-mode-map (kbd "C-c C-k") 'racket-test))))

(add-hook 'scheme-mode-hook #'enable-paredit-mode)
