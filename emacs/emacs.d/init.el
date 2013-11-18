(defvar *emacs-load-start* (current-time))

(defun add-to-load-path-list (fn)
  (add-to-list 'load-path (expand-file-name fn)))

(add-to-load-path-list "~/.emacs.d/elisp")
(add-to-load-path-list "~/.emacs.d/vendor/coffee-mode")
(add-to-load-path-list "~/.emacs.d/plugins/yasnippet")

(menu-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default show-trailing-whitespace t)
(setq-default fill-column 80)

;; This is so the edit-with-emacs-from-chrome extension will work but despite
;; the fact that it's test-edit-server seems to correctly detect whether one is
;; running or not I have not yet been able to successfully edit a document with
;; it.
(require 'edit-server)
(edit-server-start)

(require 'coffee-mode)

(require 'fill-column-indicator)

(require 'yasnippet)
(yas--initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
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



(add-hook 'python-mode-hook 'fci-mode)

;; Pretty Font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "apple" :family "Monaco")))))
(load-theme 'deeper-blue)

;; Maximize window on startup
(load "frame-cmds.el")
(maximize-frame-vertically)
(maximize-frame-horizontally)

;; http://www.emacswiki.org/emacs/BackupDirectory
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
(defun nrepl-emit-output (buffer string &optional bol)
  "Using BUFFER, emit STRING.
   If BOL is non-nil, emit at the beginning of the line."
  (with-current-buffer buffer
    (nrepl-emit-output-at-pos buffer string nrepl-input-start-mark bol)
    (ansi-color-apply-on-region (marker-position nrepl-output-start) (point-max))))

;; Marmalade Package Manager
;; http://marmalade-repo.org/about
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      cl
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

;; http://www.emacswiki.org/emacs/PareditCheatsheet
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))))
