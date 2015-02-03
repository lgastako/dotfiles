;; Python autocompletion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Fix indenting
(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

;; In theory 'C-h S' will lookup symbols, but even after installing the python
;; info files per https://bitbucket.org/jonwaltman/pydoc-info/ it still doesn't
;; quite work for me.
(require 'pydoc-info)

;; Causing problems. Very annoying.
;;(add-hook 'python-mode-hook 'fci-mode)

;;(require 'ipython)


;; ===============================================
;; from http://www.reddit.com/r/emacs/comments/24l8f2/beginner_setting_up_emacs_for_python

;; would be cool but doesn't have right pythonpath when i tried it..

;; (defun my-python-f5 ()
;;   (interactive)
;;   (python-shell-send-buffer)
;;   (python-shell-switch-to-shell))

;; (eval-after-load "python"
;;   '(progn
;;      (define-key python-mode-map (kbd "<f5>") 'my-python-f5)
;;      (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)))
;; ===============================================


;; cython
(require 'cython-mode)
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
