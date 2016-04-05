;;; free-keys-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "free-keys" "../../../../../.emacs.d/elpa/free-keys-20151202.322/free-keys.el"
;;;;;;  "a095f6a62ab5c515e2c33ee87e8692dc")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/free-keys-20151202.322/free-keys.el

(autoload 'free-keys "free-keys" "\
Display free keys in current buffer.

A free key is a key that has no associated key-binding as
determined by function `key-binding'.

By default, keys on `free-keys-keys' list with no prefix sequence
are considered, possibly together with modifier keys from
`free-keys-modifiers'.  You can change the prefix sequence by
hitting 'p' in the *Free keys* buffer.  Prefix is supplied in
format recognized by `kbd', for example \"C-x\".

\(fn &optional PREFIX BUFFER)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/free-keys-20151202.322/free-keys-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/free-keys-20151202.322/free-keys.el")
;;;;;;  (22275 17380 216928 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; free-keys-autoloads.el ends here
