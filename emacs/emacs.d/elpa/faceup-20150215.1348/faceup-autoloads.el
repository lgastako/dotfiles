;;; faceup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "faceup" "../../../../../.emacs.d/elpa/faceup-20150215.1348/faceup.el"
;;;;;;  "6cd348920102c5191433e1152181aacd")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/faceup-20150215.1348/faceup.el

(autoload 'faceup-view-buffer "faceup" "\
Display the faceup representation of the selected buffer.

\(fn)" t nil)

(autoload 'faceup-write-file "faceup" "\
Save the faceup representation of the current buffer to the file FILE-NAME.

Unless a name is given, the file will be named xxx.faceup, where
xxx is the file name associated with the buffer.

\(fn &optional FILE-NAME)" t nil)

(autoload 'faceup-render-view-buffer "faceup" "\
Convert BUFFER containing Faceup markup to a new buffer and display it.

\(fn &optional BUFFER)" t nil)

(autoload 'faceup-clean-buffer "faceup" "\
Remove faceup markup from buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/faceup-20150215.1348/faceup-autoloads.el"
;;;;;;  "../../../../../.emacs.d/elpa/faceup-20150215.1348/faceup.el")
;;;;;;  (22275 16905 521122 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; faceup-autoloads.el ends here
