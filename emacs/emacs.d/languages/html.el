;; (use-package mkhtml-htmlize)
;; (use-package mkhtml)

(use-package zencoding-mode
  ;; Auto-start on any markup modes
  :config (add-hook 'sgml-mode-hook 'zencoding-mode))
