;;; psci-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "psci" "psci.el" (23118 64370 296788 811000))
;;; Generated autoloads from psci.el

(autoload 'psci "psci" "\
Run an inferior instance of `psci' inside Emacs.
Relies on .psci file for determining the project's root folder.

\(fn PROJECT-ROOT-FOLDER)" t nil)

(autoload 'psci-mode "psci" "\
Major mode for `run-psci'.

\\<psci-mode-map>

\(fn)" t nil)

(autoload 'psci/load-current-file! "psci" "\
Load the current file in the psci repl.

\(fn)" t nil)

(autoload 'psci/load-module! "psci" "\
Load the module inside the repl session.

\(fn)" t nil)

(autoload 'psci/reset! "psci" "\
Reset the current status of the repl session.

\(fn)" t nil)

(autoload 'psci/quit! "psci" "\
Quit the psci session.

\(fn)" t nil)

(autoload 'inferior-psci-mode "psci" "\
psci minor mode to define default bindings.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("psci-dev.el" "psci-pkg.el") (23118 64370
;;;;;;  294030 649000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; psci-autoloads.el ends here
