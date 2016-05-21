;;; init.el --- John's Emacs Configuration

;; If a package appears in a use-package declartion but is disabled that means
;; that I still want it, but it was preventing init.el from loading and I
;; haven't had time to fix it yet.  Feel free to submit pull requests :)

;; Bind a key to edit this file
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file "~/dotfiles/emacs/emacs.d/init.el")))

(setq backup-by-copying         t    ;; don't clobber symlinks
      backup-directory-alist
        '(("." . "~/.saves"))        ;; collect all saves in one place
      delete-old-versions       t
      inhibit-splash-screen     t
      inhibit-startup-message   t
      kept-new-versions         6
      kept-old-versions         2
      ring-bell-function        'ignore
      ;; use-package-always-ensure t
      visible-bell              t
      version-control           t)   ;; use versioned backups

(custom-set-faces
 '(default ((t (:inherit        nil
                :stipple        nil
                :inverse-video  nil
                :box            nil
                :strike-through nil
                :overline       nil
                :underline      nil
                :slant          normal
                :weight         normal
                :height         235
                :width          normal
                :foundry        "apple"
                :family         "Monaco")))))

(if window-system
    (load-theme 'deeper-blue)
  (load-theme 'wombat t))

;; We do this right after the theme is loaded to minimize the time it looks
;; wonky.
;; (if (> (x-display-pixel-width) 2000)
;;     (set-face-attribute 'default nil :height 235)
;;   (set-face-attribute 'default nil :height 172))
(set-face-attribute 'default nil :height 172)
;; (set-face-attribute 'default nil :height 200)
;;(set-face-attribute 'default nil :height 256)
;;(set-face-attribute 'default nil :height 240)

;; Declutter the UI by hiding the menus
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Highlight the current line
(global-hl-line-mode t)
(set-face-background 'hl-line "grey9")
;; for a list of colors: http://raebear.net/comp/emacscolors.html

;; Answer 'y' or 'n' instead of 'yes' or 'no'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 All The Things
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Highlight matching delimiters
(show-paren-mode 1)

;; Never tabs
(setq-default indent-tabs-mode nil)

;; Prevent insertion of tabs for spaces
(setq-default indent-tabs-mode nil)

(defun my/haskell-cabal-mode-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'haskell-cabal-mode-hook 'my/haskell-cabal-mode-hook)

(setq-default tab-width 4)

;; Show (line, col) in modeline
(setq-default line-number-mode t
              column-number-mode t)

;; Highlight trailing whitespace in red in all modes except the ones explicitly
;; exempted below.
(setq-default show-trailing-whitespace t)
(add-hook 'cider-repl-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'buffer-menu-mode-hook (lambda () (setq show-trailing-whitespace nil)))

;; Set the print margin
(setq-default fill-column 79)

(setq auto-save-visited-file-name nil)
;; (setq auto-save-visited-file-name t)
;; (setq auto-save-interval 5)  ; keystrokes
;; (setq auto-save-timeout 5)   ; seconds

;; Automatically reverts all buffers every 5 seconds
;; Keeps us in sync with the filesystem.
(global-auto-revert-mode t)

;; Remember state of emacs when reopening.
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)

;; Save the current window configuration with 'C-x S'
(define-key ctl-x-map "S" 'save-current-configuration)
;; Restore the window configuration with 'C-x F'
(define-key ctl-x-map "F" 'resume)
;; Forget the stored window configuration with 'C-x K'
(define-key ctl-x-map "K" 'wipe)

;; Make '^x s' behave the same as '^x ^s'
(define-key ctl-x-map "s" 'save-buffer)

;; Use shift-arrow keys to move between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Don't use outdated compiled elisp
(setq load-prefer-newer t)

;; A macro for renaming mode names in the modeline
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; Packaging (brought to you by use-package)
(require' package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("marmalade"    . "https://marmalade-repo.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "Eλ")))

;; The automatic maximize-frame in this stopped working when I installed
;; projectile (and continues to fail even if I comment out the use-package for
;; projectile).  TODO: Figure out why and fix it.

;; Frame cmds is loaded as early as possible in order to
;; maximize as early as possible.
(use-package frame-cmds
  :pin melpa-stable
  :bind (("C-c f m" . maximize-frame)
         ("C-c f r" . restore-frame)
         ("C-c f o" . other-window-or-frame)
         ("<M-up>" . move-frame-up)
         ("<M-down>" . move-frame-down)
         ("<M-left>" . move-frame-left)
         ("<M-right>" . move-frame-right))
  :config
  ;; Maximize window on startup
  (maximize-frame))

(use-package revive
  :pin melpa-stable)

(use-package ido
  :pin melpa-stable
  :config (ido-mode t))

(use-package rainbow-delimiters
  :pin melpa-stable
  ;; For all programming modes:
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flymake-cursor
  :pin melpa-stable)

(use-package ws-trim
  :pin melpa-stable
  :init   (setq ws-trim-global-modes t)
  :config (global-ws-trim-mode t))

(use-package edit-server
  :pin melpa-stable
  :if window-system
  :init
  ;;  (add-hook 'after-init-hook 'server-start t)
  ;;  (add-hook 'after-init-hook 'edit-server-start t)
  )

(use-package fill-column-indicator
  :pin melpa-stable)

;; Better uniqification of buffer names
;; doesn't work with use-package for some reason
(require 'uniquify)
;; (use-package uniquify
;;   :ensure nil
;;   :config (setq uniquify-buffer-name-style 'forward))

(use-package multiple-cursors
  :pin melpa-stable
  :config
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package yasnippet
  :pin melpa-stable
  :load-path "~/.emacs.d/plugins/yasnippet"
  :config
  (setq yas/root-directory "~/.emacs.d/snippets")
  (yas-load-directory yas/root-directory)
  (yas-global-mode 1)
  (yas-reload-all))

(use-package dockerfile-mode
  :pin melpa-stable
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package popup
  :pin melpa-stable)

(use-package auto-complete
  :pin melpa-stable)

;; Require for helm
(use-package async
  :pin melpa-stable)

(use-package helm
  ;; Why isn't diminishing helm working?
  :diminish ""
  :config

  (use-package helm-git-grep)

  (require 'helm-config)

  (setq helm-quick-update                     t ; do not display invisible candidates
        helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i")   'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")   'helm-select-action)             ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; My muscle memory is set to "C-x-b" for selecting buffers, so lets change that to helm:
  (global-set-key (kbd "C-x C-b") 'list-buffers)
  (global-set-key (kbd "C-x b") 'helm-mini)



  (helm-mode 1)

  (use-package ac-helm
    :bind (("C-c g"   . helm-git-grep)
           ("C-c C-g"   . helm-git-grep)
           ("C-c t"   . helm-git-grep-at-point)
           ("C-x C-f" . helm-find-files)
           ("C-c M-i" . heml-swoop))
    :config
    (global-set-key (kbd "C-;") 'ac-complete-with-helm)
    (define-key ac-complete-mode-map (kbd "C-;") 'ac-complete-with-helm)
    ;; TODO: Does company mode supersede this?
    (auto-complete-mode))

  ;; To consider:
  ;; https://github.com/emacs-helm/helm-cmd-t

  ;; (global-set-key (kbd "C-c C-g") 'helm-google)

  ;; /sudo::/etc/hosts doesn't work for some reason.

  ;; (defadvice helm-find-files (after find-file-sudo activate)
  ;;   "Find file as root if necessary."
  ;;   (unless (and buffer-file-name
  ;;                (file-writable-p buffer-file-name))
  ;;     (find-alternate-file (concat "/sudo::" buffer-file-name))))

  ;; Invoke `helm-git-grep' from isearch.
  (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
  ;; Invoke `helm-git-grep' from other helm.
  (eval-after-load 'helm
    '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm)))

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :config (projectile-global-mode t))

(use-package ace-jump-mode
  :pin melpa-stable

  ;; Enable Ace Jump mode
  ;;   'C-u C-c SPC <char>' to jump to a specific char
  ;;   'C-c SPC <char>' to jump to a specific first-char
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode))
  ;; :bind (("C-c C-SPC" . ace-jump-char-mode))
  :config
  (defvar ace-jump-mode-submode-list
    '(ace-jump-char-mode
      ace-jump-word-mode
      ace-jump-line-mode)))

(use-package winner
  :pin melpa-stable
  ;; Winner mode
  ;;   'C-c left' and 'C-c right' to undo/redo changes to window settings
  :init (winner-mode))

(use-package drag-stuff
  :pin melpa-stable
  ;; 'M-N' / 'M-P' to move lines or selected groups of lines up/down
  ;; 'M-<left>' / 'M-<right>' to move words or selected regions
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(use-package company
  :pin melpa-stable
  :bind ("C-." . company-complete)
  :diminish ""
  :init
  (setq company-idle-delay 1.0)
  (global-company-mode 1)
  :config (bind-keys :map company-active-map
                     ("C-n"   . company-select-next)
                     ("C-p"   . company-select-previous)
                     ("C-d"   . company-show-doc-buffer)
                     ("<tab>" . company-complete)))

(use-package expand-region
  :pin melpa-stable
  :bind (("C-@" . er/expand-region)
         ("C-=" . er/expand-region)
         ("M-3" . er/expand-region))
  :init (delete-selection-mode))

(use-package hungry-delete
  :pin melpa-stable
  :init (global-hungry-delete-mode))

(use-package beacon
  :pin melpa-stable
  :diminish beacon-mode
  :init
  (beacon-mode 1)
  (setq beacon-push-mark 35
        beacon-color "#cccc00"))

(use-package org
  :pin melpa-stable
  :init (setq org-startup-indented t))

(use-package magit
  :pin melpa-stable
  :disabled t
  :bind (("C-c C-g C-g" . magit-status)))

(use-package which-key
  :pin melpa-stable
  :diminish which-key-mode
  :config (which-key-mode))

(use-package saveplace
  :pin melpa-stable
  :config
  (setq-default save-place t
                save-place-file "~/.emacs.d/saved-placed"))

(use-package auto-package-update
  :pin melpa-stable
  :disabled t
  :config
  (progn
    (setq auto-package-update-interval 1)
    (auto-package-update-maybe)))

(use-package free-keys
  :pin melpa-stable
  :defer t)

(use-package which-key
  :pin melpa-stable
  :defer t
  :diminish ""
  ;; :init	(after-init)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package golden-ratio
  :pin melpa-stable
  ;; :config
  ;; (golden-ratio-mode 1)
  )

(use-package mwim
  :pin melpa-stable
  :bind ("C-a" . mwim-beginning-of-code-or-line))

(use-package ace-window
  :pin melpa-stable
  :bind ("M-p" . ace-window)
  :config
  (setq aw-background t))

;; Doesn't work with something else?
(use-package rainbow-mode
  :pin melpa-stable
  :diminish (rainbow-mode . "")
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;;===========
;; Languages

;; clojure/clojurescript

(use-package cider
  :pin melpa-stable)

(use-package clojure-mode
  :pin melpa-stable
  :mode (("\\.clj$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljc$" . clojurec-mode)
         ("\\.cljx$" . clojurex-mode)
         ("\\.edn$" . clojure-mode))
  :init

  ;; Fix indenting on some things
  (add-hook 'clojure-mode-hook
            (lambda ()
              (put-clojure-indent 'defui '(1 nil nil (1)))))

  ;; Why doesn't this work?
  ;; See https://github.com/clojure-emacs/clojure-mode
  ;; And https://github.com/clojure-emacs/cider/blob/master/doc/Indent-Spec.md#indent-specification
  (add-hook 'clojure-mode-hook
            (lambda ()
              (put-clojure-indent 'defcomponent '(1 nil nil (1)))))

  ;; Clojure Files
  ;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

  ;; In the REPL
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  ;; (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

  (setq exec-path (append exec-path '("/Users/john/dotfiles/bin"))
        cider-lein-command "/Users/john/dotfiles/bin/lein"
        cider-boot-command "/Users/john/dotfiles/bin/boot")

  (setq cider-overlays-use-font-lock t)

  ;; (setq cider-auto-select-error-buffer t)

  :config
  (rename-modeline "clojure-mode" clojure-mode "λ"))

(use-package clj-refactor
  :pin melpa-stable
  :init   (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
  :config (cljr-add-keybindings-with-prefix "C-!"))

;; coffescript

(use-package coffee-mode
  :pin melpa-stable
  :mode "\\.coffee\\'"
  :config
  (defun coffee-tabs ()
    (setq coffee-indent-tabs-mode t)
    (setq indent-tabs-mode t)
    (setq coffee-tab-width 4))
  (add-hook 'coffee-mode-hook 'coffee-tabs))

;; elixir

(use-package elixir-mode
  :pin melpa-stable
  :config
  (use-package alchemist
    :pin melpa-stable
    :diminish alchemist-mode
    ;; :ensure t
    ))

;; elm
(use-package elm-mode
  :pin melpa-stable
  :init
  (setq elm-format-on-save t)
  (setq elm-indent-offset 4)
  (setq elm-interactive-arguments '("--interpreter=/usr/local/bin/node"))
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm))

;; erlang

(use-package erlang
  :pin melpa-stable
  :disabled t        ;; causes slow load
  )

;; go

(use-package go-mode
  :pin melpa-stable
  :init
  (setenv "GOPATH" "~/go")
  ;; These next two should probably be moved out to the top level, no?
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq exec-path (append exec-path '("/usr/local/go/bin")))
  (setq exec-path (append exec-path '("~/go/bin")))

  :config

  (use-package flymake-go
    :pin melpa-stable
    :init
    (setq gofmt-command "goimports")
    :config
    (add-hook 'before-save-hook 'gofmt-before-save))

  (use-package go-eldoc
    :pin melpa-stable
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

;; haml

(use-package haml-mode
  :pin melpa-stable
  :mode "\\.haml\\'")

;; haskell

(use-package haskell-mode
  :pin melpa-stable
  :mode ("\\.hs\\'" "\\.lhs\\'")
  ;; :mode "\\.\\(?:[gh]s\\|hi\\)\\'"

  :init
  ;; We need to establish both an interaction mode and an indentation mode.
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (setq haskell-process-type 'stack))

;; html

(use-package mkhtml-htmlize
  :pin melpa-stable
  :disabled t)

(use-package mkhtml
  :pin melpa-stable
  :disabled t)

(use-package zencoding-mode
  :pin melpa-stable
  ;; Auto-start on any markup modes
  :config (add-hook 'sgml-mode-hook 'zencoding-mode))

;; javascript

(use-package js2-mode
  :pin melpa-stable
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode)
         ("\\.babelrc$" . js2-mode))
  :interpreter ("node" . js2-mode)
  ;; :bind (("C-a" . back-to-indentation-or-beginning-of-line)
  ;;        ("C-M-h" . backward-kill-word))
  :config
  (progn
;;    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    ;; (add-hook 'js2-mode-hook (lambda ()
    ;;                            (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))
    ))


;; json

(use-package json-reformat
  :pin melpa-stable
  :disabled t)

(use-package json-mode
  :pin marmalade)

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(global-set-key (kbd "C-x j") 'json-format)

(use-package markdown-mode
  :pin melpa-stable
  :mode ("\\.md\\'" "\\.markdown\\'"))

;; nu

;; wtf, nu-mode?
(use-package nu-mode
  :pin melpa-stable
  :disabled t
  :mode ("\\.nu\\'" . nu-mode)
;;  :config (nu-mode 1)
  )

;; ocaml

(use-package tuareg
  :pin melpa-stable
  :mode "\\.ml\\'"
  :config
  (use-package utop
    :pin melpa-stable)
  (use-package merlin
    :pin melpa-stable))

;; python

(use-package python
  :pin melpa-stable
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)

  :config

  ;; Python autocompletion
  ;;(add-hook 'python-mode-hook 'jedi:setup)
  ;;(setq jedi:complete-on-dot t)

  ;; Fix indenting
  (add-hook 'python-mode-hook
            '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

  ;; In theory 'C-h S' will lookup symbols, but even after installing the python
  ;; info files per https://bitbucket.org/jonwaltman/pydoc-info/ it still doesn't
  ;; quite work for me.
  (use-package pydoc-info
    :pin melpa-stable)

  ;; Causing problems. Very annoying.
  ;;(add-hook 'python-mode-hook 'fci-mode)

  (use-package ipython
    :pin melpa-stable)

  ;; ===============================================
  ;; from http://www.reddit.com/r/emacs/comments/24l8f2/beginner_setting_up_emacs_for_python

  ;; would be cool but doesn't have right pythonpath when i tried it..

  ;; (defun my-python-f5 ()
  ;;   (interactive)
  ;;   (python-shell-send-buffer)
  ;;   (python-shell-switch-to-shell))

  ;; (eval-after-load "python"
  ;;   '(progn
  ;;      (define-key python-mode-map (kbd "<f5>") 'my-python-f5)
  ;;      (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)))
  ;; ===============================================

  (use-package virtualenvwrapper
    :pin melpa-stable
    ;; :disabled t
    :commands (venv-workon))

  (use-package cython-mode
    :pin melpa-stable
    :config (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))))

;; REST

(use-package restclient
  :pin melpa-stable
  :mode ("\\.rest\\'" . restclient-mode))

;; ruby

(use-package ruby-mode
  :pin melpa-stable
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
  (use-package el4r
    :pin melpa-stable
    :disabled t
    :config
    (el4r-boot)
    (el4r-troubleshooting-keys)))

;; rust

(use-package rust-mode
  :pin melpa-stable
  :mode "\\.rs\\'")

;; salesforce

;; (use-package apex-mode
;;   :pin melpa-stable)
;; (require 'apex-mode)

(add-to-list 'auto-mode-alist '("\\.visualforcepage$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vfc$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.apexclass$" . js-mode))

;; sass

(use-package sass-mode
  :pin melpa-stable
  :mode ("\\.sass\\'" "\\.scss\\'"))

;; sbcl (common lisp)

(use-package sly
  :pin melpa-stable
  :commands (sly)
  :config (setq inferior-lisp-program (executable-find "sbcl")))

;; scala

(use-package scala-mode2
  :pin melpa-stable)

;; fuck this noise
;; (use-package sbt-mode
;;   :pin melpa-stable
;;   :mode "\\.sbt\\'"
;;   :init
;;   (setq exec-path (append exec-path '("/usr/local/bin"))))

;; scheme

(use-package quack
  :pin melpa-stable)

(use-package geiser
  :pin melpa-stable
  :mode "\\.scm\\'"
  :config
  (setq geiser-active-implementations '(racket))
  (setq geiser-racket-binary "/Users/john/local/bin/racket"))

;; (require 'faceup)
(use-package faceup
  :pin melpa-stable)

(use-package racket-mode
  :pin melpa
  :defer t
  :config
  (add-hook 'racket-mode-hook
            '(lambda ()
               (define-key racket-mode-map (kbd "C-c C-l") 'racket-run)
               (define-key racket-mode-map (kbd "C-c C-k") 'racket-test))))

;; swift

(use-package swift-mode
  :pin melpa-stable)

;; YAML

(use-package yaml-mode
  :pin melpa-stable
  :mode "\\.yml\\'")

(use-package flycheck
  :disabled t
  :pin melpa-stable
  :init
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(use-package paredit
  :pin melpa
  :diminish "par"
  :init
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'go-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (global-set-key (kbd "C-x p") 'paredit-mode)

  (add-to-list 'load-path "~/.emacs.d/lisp")
  ;; (require 'simple-secrets)
  ;; (require 'secret-funs)

  ;; (global-set-key (kbd "C-c s n")
  ;;                 (create-new-site-secret))

  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
  (global-set-key (kbd "C-c l")   'linum-mode)
  (global-set-key (kbd "C-c C-l") 'global-linum-mode)

  ;; Type greek lambda character with "M-g l"
  ;; Dunno why these two don't work.
  ;; (global-set-key (kbd "M-g c") "©")
  ;; (global-set-key (kbd "M-g t") "™")
  (global-set-key (kbd "M-g l") "λ")
  (global-set-key (kbd "M-g d") "Δ")
  (global-set-key (kbd "M-g - >") "→")
  (global-set-key (kbd "M-g = >") "⇒")
  (global-set-key (kbd "M-g f") "∀")
  (global-set-key (kbd "M-g e") "∃")

  (setq erc-track-enable-keybindings nil)

  (put 'upcase-region   'disabled nil)
  (put 'downcase-region 'disabled nil)

  (defun rotate-windows (arg)
    "Rotate your windows; use the prefix argument to rotate the
     other direction.  Additionally given a numeric prefix argument n,
     it will rotate the windows n times; if the numeric argument is
     negative rotates |n| times in the other direction."
    (interactive "P")
    (if (not (> (count-windows) 1))
        (message "You can't rotate a single window!")
      (let* ((rotate-times (prefix-numeric-value arg))
             (direction (if (or (< rotate-times 0) (equal arg '(4)))
                            'reverse 'identity)))
        (dotimes (_ (abs rotate-times))
          (dotimes (i (- (count-windows) 1))
            (let* ((w1 (elt (funcall direction (window-list)) i))
                   (w2 (elt (funcall direction (window-list)) (+ i 1)))
                   (b1 (window-buffer w1))
                   (b2 (window-buffer w2))
                   (s1 (window-start w1))
                   (s2 (window-start w2))
                   (p1 (window-point w1))
                   (p2 (window-point w2)))
              (set-window-buffer-start-and-point w1 b2 s2 p2)
              (set-window-buffer-start-and-point w2 b1 s1 p1))))))))

(global-set-key (kbd "C-x r w") 'rotate-windows)
(global-set-key (kbd "C-x w r") 'rotate-windows)

;; Experimental
(global-set-key (kbd "C-c r a t") 'mc/mark-all-like-this-dwim)

;; ;; TODO: Figure out how to send this to figwheel REPLs automatically:
;; (use 'figwheel-sidecar.repl-api)
;; (start-figwheel!)
;; ;; Then once that's started...
;; (cljs-repl)
(put 'erase-buffer 'disabled nil)
