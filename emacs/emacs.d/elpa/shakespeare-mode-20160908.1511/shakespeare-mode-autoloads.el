;;; shakespeare-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "shakespeare-mode" "shakespeare-mode.el" (22567
;;;;;;  23316 0 0))
;;; Generated autoloads from shakespeare-mode.el

(autoload 'shakespeare-mode "shakespeare-mode" "\
Shakespeare Mode minor mode for keymaps and mode-hooks.

\(fn &optional ARG)" t nil)

(autoload 'shakespeare-hamlet-mode "shakespeare-mode" "\
A major mode for shakespearean hamlet files.
  \\{shakespeare-mode-map}

\(fn)" t nil)

(autoload 'shakespeare-lucius-mode "shakespeare-mode" "\
A major mode for shakespearean lucius files.
  \\{shakespeare-mode-map}

\(fn)" t nil)

(autoload 'shakespeare-julius-mode "shakespeare-mode" "\
A major mode for shakespearean julius files.
  \\{shakespeare-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . shakespeare-hamlet-mode))

(add-to-list 'auto-mode-alist '("\\.lucius\\'" . shakespeare-lucius-mode))

(add-to-list 'auto-mode-alist '("\\.julius\\'" . shakespeare-julius-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; shakespeare-mode-autoloads.el ends here
