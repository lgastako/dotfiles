(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :init
  (setenv "PATH"
          (concat
           (getenv "HOME") "/.rvm/rubes/ruby-1.9.3-p448/bin:"
           (getenv "PATH")))
  ;; Load el4r, which loads Xiki
  (add-to-list 'load-path "/Library/Ruby/Gems/2.0.0/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
  ;;(add-to-list 'load-path "~/.rvm/gems/ruby-1.9.3-p448/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
  ;; (use-package el4r
  ;;   :config
  ;;   (el4r-boot)
  ;;   (el4r-troubleshooting-keys))
  )
