(use-package markdown-mode
  :config
  (setq auto-mode-alist
        (cons '("\\.md" . markdown-mode)
              auto-mode-alist)))
