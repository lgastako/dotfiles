(add-hook 'python-mode-hook 'fci-mode)

(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

