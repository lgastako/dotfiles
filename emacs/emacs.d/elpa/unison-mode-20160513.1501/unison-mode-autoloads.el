;;; unison-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "unison-mode" "unison-mode.el" (0 0 0 0))
;;; Generated autoloads from unison-mode.el

(autoload 'unison-mode "unison-mode" "\
Major mode for font-lcoking unison configuration files

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.prf$" . unison-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "unison-mode" '("unison-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; unison-mode-autoloads.el ends here
