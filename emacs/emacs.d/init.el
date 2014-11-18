(defvar *emacs-load-start* (current-time))

(defun add-to-load-path-list (fn)
  (add-to-list 'load-path (expand-file-name fn)))

(add-to-load-path-list "~/.emacs.d/elisp")
(add-to-load-path-list "~/.emacs.d/vendor/coffee-mode")
(add-to-load-path-list "~/.emacs.d/plugins/yasnippet")
(add-to-load-path-list "/usr/local/Cellar/go/1.0.3/misc/emacs")
(add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")

;; bah humbug - too slow, doesn't pay for itself
;;(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(menu-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default show-trailing-whitespace t)
(setq-default fill-column 80)
(setq auto-save-visited-file-name nil)
;; (setq auto-save-visited-file-name t)
;; (setq auto-save-interval 5)  ; keystrokes
;; (setq auto-save-timeout 5)   ; seconds
;; (setq cider-auto-select-error-buffer t)

(global-auto-revert-mode t)

;; Remember state of emacs when reopening.
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

(define-key ctl-x-map "S" 'save-current-configuration)
(define-key ctl-x-map "F" 'resume)
(define-key ctl-x-map "K" 'wipe)

;; Make '^x s' behave the same as '^x ^s'
(define-key ctl-x-map "s" 'save-buffer)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(require 'haml-mode)
(require 'sass-mode)

;; (require 'mkhtml-htmlize)
;; (require 'mkhtml)

;; (require 'easymenu)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'rainbow-delimiters)
;; For specific modes:
;;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; For all programming modes:
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;(add-hook 'clojure-mode-hook 'typed-clojure-mode)

(require 'theme-park-mode)

(require 'flymake-cursor)

(require 'go-mode-load)
;;(require 'go-flymake)

(require 'ws-trim)
(global-ws-trim-mode t)
(setq ws-trim-global-modes t)

(require 'edit-server)
(edit-server-start)

(require 'coffee-mode)

(require 'rust-mode)

(require 'fill-column-indicator)

(require 'uniquify)

(require 'yasnippet)
(yas--initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas-load-directory yas/root-directory)
(yas-global-mode 1)

;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile (concat "lein kibit " buffer-file-name)))

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c k") 'kibit-current-file)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x p") 'paredit-mode)

(defun indent-all ()
  (interactive)
  (indent-region 0 (buffer-size)))

(global-set-key (kbd "C-c f") 'indent-all)

;;(global-set-key (kbd "C-c t") 'nrepl-make-repl-connection-default)

(add-hook 'python-mode-hook 'fci-mode)

(setq gofmt-command "goimports")

(add-hook 'before-save-hook 'gofmt-before-save)

;; Pretty Font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 235 :width normal :foundry "apple" :family "Monaco")))))
(load-theme 'deeper-blue)

;; (if (> (x-display-pixel-width) 2000)
;;     (set-face-attribute 'default nil :height 235)
;;   (set-face-attribute 'default nil :height 172))
;; (set-face-attribute 'default nil :height 172)
;; (set-face-attribute 'default nil :height 200)
(set-face-attribute 'default nil :height 256)
;;(set-face-attribute 'default nil :height 240)

;;http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t       ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)        ; use versioned backups

;; Fix problem with nrepl and ANSI colors
;; See https://github.com/clojure-emacs/cider/issues/312
;; and https://github.com/clojure-emacs/cider/pull/275
;; (defun nrepl-emit-output (buffer string &optional bol)
;;   "Using BUFFER, emit STRING.
;;    If BOL is non-nil, emit at the beginning of the line."
;;   (with-current-buffer buffer
;;     (nrepl-emit-output-at-pos buffer string nrepl-input-start-mark bol)
;;     (ansi-color-apply-on-region (marker-position nrepl-output-start) (point-max))))

;; Fix problem with nrepl not switching to init-ns from project.clj
;; See https://github.com/clojure-emacs/cider/issues/316 (though this is for
;; Cider which I am not using... yet)
;; (add-hook 'nrepl-connected-hook
;;   (lambda () (nrepl-set-ns (plist-get
;;                  (nrepl-send-string-sync "(symbol (str *ns*))") :value))))

;; Use arrows to recall previous/next commands in nrepl instead of just M-n/p
;; (define-key nrepl-mode-map (kbd "<up>") 'nrepl-previous-input)
;; (define-key nrepl-mode-map (kbd "<down>") 'nrepl-next-input)
;; This breaks when you're trying to up/down arrow through text anywhere in the
;; buffer.

;; Marmalade Package Manager
;; http://marmalade-repo.org/about
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(cl
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.cljx$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\.edn$" . clojure-mode))

;; http://www.emacswiki.org/emacs/PareditCheatsheet
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'go-mode-hook               #'enable-paredit-mode)

(autoload 'markdown-mode "markdown-mode.el"	"Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(haskell-mode-hook (quote (paredit-mode capitalized-words-mode turn-on-haskell-decl-scan turn-on-haskell-doc turn-on-haskell-indent turn-on-haskell-indentation turn-on-haskell-simple-indent)))
 '(helm-google-search-function (quote helm-google-api-search))
 '(quack-programs (quote ("/Users/john/local/bin/racket" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme" "mzschme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))
 '(tpm-tagged nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;
;; Haven't properly merged these into the appropriate spots above.

;; Maximize window on startup
(load "frame-cmds.el")
(maximize-frame-vertically)
(maximize-frame-horizontally)

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key "\C-y" 'yank-and-indent)

(load-library "troncle")

(setenv "PATH" (concat (getenv "PATH") ":$HOME/bin"))
(setq sql-postgres-program "/usr/local/bin/psql")
;;(setq sql-port ...) -- seems to not be a good idea
;; this seems to be the way: sigh
;;(setq sql-postgres-options (list "-p 5492"))

(require 'quack)
(require 'geiser)
(setq geiser-active-implementations '(racket))
(setq geiser-racket-binary "/Users/john/local/bin/racket")

;;failed to compile
;;(load-file "/Users/john/.emacs.d/elisp/ProofGeneral/generic/proof-site.el")

(setenv "GOPATH" "~/go")

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/local/go/bin")))
(setq exec-path (append exec-path '("~/go/bin")))

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(setenv "PATH"
        (concat
         (getenv "HOME") "/.rvm/rubes/ruby-1.9.3-p448/bin:"
         (getenv "PATH")))

; Load el4r, which loads Xiki
(add-to-list 'load-path "/Library/Ruby/Gems/2.0.0/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
;;(add-to-list 'load-path "~/.rvm/gems/ruby-1.9.3-p448/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
;; (require 'el4r)
;; (el4r-boot)
;; (el4r-troubleshooting-keys)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; (eval-after-load 'haskell
;;   '(define-key ))

;; (add-hook 'haskell-mode
;;           (lambda () (local-set-key (kbd "C-c") #'haskell-compile)))

(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; My muscle memory is set to "C-x-b" for selecting buffers, so lets change that to helm:
(global-set-key (kbd "C-x C-b") 'list-buffers)
(global-set-key (kbd "C-x b") 'helm-mini)

(helm-mode 1)

(require 'ac-helm)
(global-set-key (kbd "C-;") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-;") 'ac-complete-with-helm)

(global-set-key (kbd "C-c C-g") 'helm-google)

(auto-complete-mode)

;; To consider:
;; https://github.com/clojure-emacs/ac-cider
;; https://github.com/emacs-helm/helm-cmd-t

;; Bind a key to edit my init.el
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file "~/dotfiles/emacs/emacs.d/init.el")))

(add-hook 'python-mode-hook
   '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

;; (require 'helm-git-grep) ;; Not necessary if installed by package.el
(global-set-key (kbd "C-c t") 'helm-git-grep-at-point)
(global-set-key (kbd "C-c g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))
