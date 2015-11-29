(provide 'secret-funs)

(use-package dash)

(defun kw->name (kw)
  ;; (message (concat "kw: " (symbol-name kw)))
  (substring (symbol-name kw) 1))

;; (kw->name :foo) ;; => foo

(defun kws->names (&rest keywords)
  (mapcar 'kw->name keywords))

;; (kws->names :foo :bar :baz) ;; => ("foo" "bar" "baz")

(defun serialize-config (config)
  (let ((type (substring (kw->name (car config)) 0 1))
        (label (cadr config)))
    (concat type label)))

;; (serialize-config '(:str "Username"))    ;; => "sUsername"
;; (serialize-config '(:str "Password"))    ;; => "sPassword"
;; (serialize-config '(:num "Dexterity"))   ;; => "nDexterity"

;; (mapcar 'serialize-config
;;         '((:str "Username")
;;           (:num "Foo")))    ;; => ("sUsername" "nFoo")

;; (defun prompts (&rest configs)
;;   (apply 'concat
;;          (-interpose "\n"
;;                      (nconc (mapcar 'serialize-config configs)))))

(defun prompts (&rest labels)
  (mapcar (lambda (label)
            (read-input label))
          labels))

;; (prompts "Username" "Foo")

(defun create-new-site-secret (site username password)
  (interactive (prompts "Site: "
                        "Username: "
                        "Password: "))
  (secret-new (concat site "/username") username)
  (secret-new (concat site "/password") password))

;; (call-interactively 'create-new-site-secret)

;; (create-new-site-secret "foo" "Bar " "baz")

(defun ff (arg)
  "Prompt user to enter a string, with input history support."
  (interactive (list (read-string "Your name is:")))
  (message "String is %s." arg) )


;; (call-interactively)
;; (defun secret-funs (&rest rest)
;;   :not-implemented)
