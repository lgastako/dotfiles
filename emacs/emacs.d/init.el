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
(setq auto-save-visited-file-name f)
;; (setq auto-save-visited-file-name t)
;; (setq auto-save-interval 5)  ; keystrokes
;; (setq auto-save-timeout 5)   ; seconds
(global-auto-revert-mode t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


(require 'ws-trim)
(global-ws-trim-mode t)
(setq ws-trim-global-modes t)

(require 'edit-server)
(edit-server-start)

(require 'coffee-mode)

(require 'fill-column-indicator)

(require 'uniquify)

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

(global-set-key (kbd "C-c k") 'kibit-current-file)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c C-j") 'nrepl-jack-in)
(global-set-key (kbd "C-x p") 'paredit-mode)

(add-hook 'python-mode-hook 'fci-mode)

;; Pretty Font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 235 :width normal :foundry "apple" :family "Monaco")))))
(load-theme 'deeper-blue)

;; Temporary, really only want it on maia.local.
;;(set-face-attribute 'default nil :height 235)

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

;; Fix problem with nrepl not switching to init-ns from project.clj
;; See https://github.com/clojure-emacs/cider/issues/316 (though this is for
;; Cider which I am not using... yet)
(add-hook 'nrepl-connected-hook
  (lambda () (nrepl-set-ns (plist-get
                 (nrepl-send-string-sync "(symbol (str *ns*))") :value))))

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

(autoload 'markdown-mode "markdown-mode.el"	"Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
(put 'upcase-region 'disabled nil)
