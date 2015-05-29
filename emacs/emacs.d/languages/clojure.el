;; ;; For specific modes:

;; ;; Fix problem with nrepl and ANSI colors
;; ;; See https://github.com/clojure-emacs/cider/issues/312
;; ;; and https://github.com/clojure-emacs/cider/pull/275
;; ;; (defun nrepl-emit-output (buffer string &optional bol)
;; ;;   "Using BUFFER, emit STRING.
;; ;;    If BOL is non-nil, emit at the beginning of the line."
;; ;;   (with-current-buffer buffer
;; ;;     (nrepl-emit-output-at-pos buffer string nrepl-input-start-mark bol)
;; ;;     (ansi-color-apply-on-region (marker-position nrepl-output-start) (point-max))))

;; ;; Fix problem with nrepl not switching to init-ns from project.clj
;; ;; See https://github.com/clojure-emacs/cider/issues/316 (though this is for
;; ;; Cider which I am not using... yet)
;; ;; (add-hook 'nrepl-connected-hook
;; ;;   (lambda () (nrepl-set-ns (plist-get
;; ;;                  (nrepl-send-string-sync "(symbol (str *ns*))") :value))))

;; ;; Use arrows to recall previous/next commands in nrepl instead of just M-n/p
;; ;; (define-key nrepl-mode-map (kbd "<up>") 'nrepl-previous-input)
;; ;; (define-key nrepl-mode-map (kbd "<down>") 'nrepl-next-input)
;; ;; This breaks when you're trying to up/down arrow through text anywhere in the
;; ;; buffer.

;; (load-library "troncle")

;; (add-hook 'clojure-mode-hook 'typed-clojure-mode)

;; ;; To consider:
;; ;; https://github.com/clojure-emacs/ac-cider


;; ;; Teach compile the syntax of the kibit output
;; (require 'compile)
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
;; (add-to-list 'compilation-error-regexp-alist 'kibit)

;; ;; A convenient command to run "lein kibit" in the project to which
;; ;; the current emacs buffer belongs to.
;; (defun kibit ()
;;   "Run kibit on the current project.
;; Display the results in a hyperlinked *compilation* buffer."
;;   (interactive)
;;   (compile "lein kibit"))

;; (defun kibit-current-file ()
;;   "Run kibit on the current file.
;; Display the results in a hyperlinked *compilation* buffer."
;;   (interactive)
;;   (compile (concat "lein kibit " buffer-file-name)))

;; (global-set-key (kbd "C-c k") 'kibit-current-file)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljx$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljc$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.boot$" . clojure-mode))

(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;;(setq cider-lein-command "/Users/john/dotfiles/bin/lein")
(setq cider-boot-command "/Users/john/dotfiles/bin/boot")



;;(global-set-key (kbd "C-c j") 'cider-jack-in)



;;(define-key cider-repl-mode (kbd "C-c o") 'cider-repl-clear-output)
;; (define-key cider-repl-mode (kbd "C-c o") (lambda ()
;;                                             (interactive)
;;                                             (cider-repl-clear-output)))
