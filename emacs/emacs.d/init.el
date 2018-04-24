;;; init.el --- John's Emacs Configuration

;; If a package appears in a use-package declartion but is disabled that means
;; that I still want it, but it was preventing init.el from loading and I
;; haven't had time to fix it yet.  Feel free to submit pull requests :)

;; Global path modifications
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/john/dotfiles/bin")))
(setq exec-path (append exec-path '("/Users/john/local/bin")))
(setq exec-path (append exec-path '("/Users/john/.local/bin")))
;; Not sure why I have to do this, but I do.
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Users/john/.local/bin"))


;; Bind a key to edit this file
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file "~/dotfiles/emacs/emacs.d/init.el")))

;; Bind a key to edit ~/.lein/profiles.clj
(global-set-key (kbd "C-c e p")
                (lambda ()
                  (interactive)
                  (find-file "~/.lein/profiles.clj")))

;; Bind a key to edit notes for Zeke's house
(global-set-key (kbd "C-c e z")
                (lambda ()
                  (interactive)
                  (find-file "~/Dropbox/org/for-zekes-house.md")))

;; Bind a key to edit ~/Dropbox/org
(global-set-key (kbd "C-c e o")
                (lambda ()
                  (interactive)
                  (find-file "~/Dropbox/org")))

;; Bind a key to edit ~/Dropbox/org
(global-set-key (kbd "C-c e w")
                (lambda ()
                  (interactive)
                  (find-file "~/src/writing")))


;; Prevent me from accidentally minimizing emacs.
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)


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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 235 :width normal :foundry "apple" :family "Monaco")))))

;; '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 235 :width normal :foundry "apple" :family "Andale Mono")))))

(if window-system
    (load-theme 'deeper-blue)
  (load-theme 'wombat t))

;; We do this right after the theme is loaded to minimize the time it looks
;; wonky.
;; (if (> (x-display-pixel-width) 2000)
;;     (set-face-attribute 'default nil :height 235)
;;   (set-face-attribute 'default nil :height 172))

;; (set-face-attribute 'default nil :height 100)
;; (set-face-attribute 'default nil :height 144)
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

;; I hate this, but too much high blood pressue to not do it
(setq confirm-kill-emacs 'y-or-n-p)

;; UTF-8 All The Things
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Revert TAGS files without asking
(setq tags-revert-without-query 1)
;; ...and generate haskell tags on save.
(setq haskell-tags-on-save t)
;; (require 'speedbar)
;; (speedbar-add-supported-extension ".hs")
;; (speedbar 1)
;; (add-to-list 'load-path "~/dotfiles/emacs/emacs.d/lisp/hasktags")
;; (load "hasktags")
(global-set-key (kbd "M-,") 'pop-tag-mark)

;; Highlight matching delimiters
(show-paren-mode 1)

;; Never tabs
(setq-default indent-tabs-mode nil)

;; Maintain case in dynamic expansions
(setq-default dabbrev-case-fold-search nil)

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
(add-hook 'term-mode-hook        (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'cider-repl-mode-hook  (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'buffer-menu-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(add-hook 'idris-repl-mode-hook  (lambda () (setq show-trailing-whitespace nil)))

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

;;(require 'diminish)
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
  :config
  (ido-mode t)
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  (setq ffap-machine-p-known 'reject))

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
(setq uniquify-buffer-name-style 'forward)

;; (use-package uniquify
;;   :config (setq uniquify-buffer-name-style 'forward))

(use-package multiple-cursors
  :pin melpa-stable
  :config
  (global-set-key (kbd "C-c m")   'mc/edit-lines)
  (global-set-key (kbd "C->")     'mc/mark-next-like-this)
  (global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
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

  (use-package helm-ag
    :bind (("C-c g"   . helm-do-ag-project-root)
           ("C-c C-g" . helm-do-ag-project-root))
    :config (setq helm-ag-use-grep-ignore-list t))

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
  (define-key helm-map (kbd "C-z")   'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (global-set-key (kbd "M-x") 'helm-M-x)

  ;; My muscle memory is set to "C-x-b" for selecting buffers, so lets change that to helm:
  (global-set-key (kbd "C-x C-b") 'list-buffers)
  (global-set-key (kbd "C-x b") 'helm-mini)

  (helm-mode 1)

  (use-package ac-helm
    :bind (("C-x C-f" . helm-find-files))
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
    '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))
  :init
  (use-package helm-idris))

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :config (projectile-global-mode t))

;; TODO: delete me
;; (print-elements-of-list exec-path)
;; (defun print-elements-of-list (list)
;;   "Print each element of LIST on a line of its own."
;;   (while list
;;     (print (car list))
;;     (setq list (cdr list))))
;; (shell-command-to-string "ag --literal --nocolor --noheading -l -- foo")
;; (concat "ag --literal --nocolor --noheading -l -- " "foo")
;; (executable-find "ag")

(use-package avy
  :bind (("C-c SPC" . avy-goto-char)
         ("C-c C-SPC" . avy-goto-char))
  ;; Replaces 'M-g g' binding of goto-line because it switches back to
  ;; goto-line automatically if you type a number...
  :config
  (global-set-key (kbd "M-g g")   'avy-goto-line)
  (global-set-key (kbd "M-g M-g") 'avy-goto-line))

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
         ("M-P" . drag-stuff-up))
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode))

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
  :init (setq org-startup-indented t)
  :config
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE"))))

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

;; This should be before any of the languages
(use-package flycheck
  :ensure t
  :pin melpa-stable
  ;; :init
  ;; (use-package flycheck-elm)
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (progn
    (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
    (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
    (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)
    ;;(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
    ))

(dumb-jump-mode)
(setq dumb-jump-selector 'helm)

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

  ;; :bind (("C-c C-k" . cider-repl-clear-buffer))
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

  (setq cider-lein-command "/Users/john/dotfiles/bin/lein"
        cider-boot-command "/Users/john/dotfiles/bin/boot")

  (setq cider-overlays-use-font-lock t)

  ;; (setq cider-auto-select-error-buffer t)

  :config
  (rename-modeline "clojure-mode" clojure-mode "λ")
  (rename-modeline "clojure-mode" clojurec-mode "λc")
  (rename-modeline "clojure-mode" clojurescript-mode "λs")

  ;; These two don't work (eg. if you use "C-x C-b")
  ;; (rename-modeline "clojurec-mode" clojure-mode "λc")
  ;; (rename-modeline "clojurescript-mode" clojure-mode "λs")
  )

;; (use-package clj-refactor
;;   :pin melpa-stable
;;   :init   (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
;;   :config (cljr-add-keybindings-with-prefix "C-!"))

;; cmake

(use-package cmake-mode
  :mode "CMakeLists.txt")

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

;; c#

(use-package csharp-mode)

(use-package csv-mode
  :ensure t)

;; elixir

(use-package elixir-mode
  :pin melpa-stable
  :config
  (use-package alchemist
    :pin melpa-stable
    :diminish alchemist-mode))

;; elm
;; (use-package elm-mode
;;   :pin melpa-stable
;;   :bind (("C-c C-k" . elm-compile-main))
;;   :init
;;   ;; Not sure why I have to set elm-compile-command explicity all of a
;;   ;; sudden...  I didn't need to do this initially, and /usr/local/bin is in my
;;   ;; exec-path properly... but, it gets the job done.
;;   (setq elm-compile-command "/usr/local/bin/elm-make")
;;   (setq elm-format-on-save t)
;;   (setq elm-indent-offset 4)
;;   (setq elm-interactive-arguments '("--interpreter=/usr/local/bin/node"))
;;   (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
;;   (add-to-list 'company-backends 'company-elm))

;; erlang

(use-package erlang
  :pin melpa-stable
  :disabled t        ;; causes slow load
  )

;; Elastic Search

(use-package es-mode
  :ensure t
  :mode "\\.es\\'")

;; go

;; (use-package go-mode
;;   :pin melpa-stable
;;   :init
;;   (setenv "GOPATH" "~/go")
;;   ;; These next two should probably be moved out to the top level, no?
;;   (setq exec-path (append exec-path '("/usr/local/go/bin")))
;;   (setq exec-path (append exec-path '("~/go/bin")))

;;   :config

;;   (use-package flymake-go
;;     :pin melpa-stable
;;     :init
;;     (setq gofmt-command "goimports")
;;     :config
;;     (add-hook 'before-save-hook 'gofmt-before-save))

;;   (use-package go-eldoc
;;     :pin melpa-stable
;;     :config
;;     (add-hook 'go-mode-hook 'go-eldoc-setup)))

;; haml

(use-package haml-mode
  :pin melpa-stable
  :mode "\\.haml\\'")

;; haskell

(use-package haskell-mode
  :pin melpa-stable
  :bind
  ("C-c m" . haskell-process-reload-devel-main)
  :init
  (setq haskell-interactive-popup-errors nil)

  (defun longboyeee-off ())
  (defun longboyeee-on ()
    (interactive "r")
    (when (eq major-mode 'haskell-mode)
      (let ((start    1)
            (end      (+ 1 (buffer-size)))
            (program "longboye"))
        (let ((saved-cursor-position (point)))
            (call-process-region start
                                 end
                                 program
                                 t         ;; delete
                                 t         ;; destination
                                 nil       ;; display
                                 "all"
                                 "-"
                                 )
            (goto-char saved-cursor-position)))))

  ;; (defun longboyeee () (longboyeee-on))
  (defun longboyeee () (longboyeee-off))

  (add-hook 'before-save-hook #'longboyeee)
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  ;;  (customize-set-variable 'haskell-process-type 'stack-ghci)

  (setq haskell-process-args-stack-ghci
        ;; '("--ghci-options='-ferror-spans'")
        '("--ghc-options=-ferror-spans"
          ;; -fshow-loaded-modules is because haskell-mode doesn't work with GHC 8.2.2 yet
          ;; see https://github.com/haskell/haskell-mode/issues/1553
          ;; "--ghc-options=-fshow-loaded-modules"
          "--test"
          ))

  ;; This -Wall -Werror doesn't seem to take affect on eg. 'C-c C-l'
  ;; (setq ghc-ghc-options '("-Wall" "-Werror" "-v"))
  ;; (setq-default haskell-stylish-on-save t)

  (add-to-list 'load-path "~/dotfiles/emacs/emacs.d/lisp")
  (require 'tidal)


  ;; (use-package dante
  ;;   :ensure t
  ;;   ;; :after haskell-mode
  ;;   :commands 'dante-mode
  ;;   :init
  ;;   (add-hook 'haskell-mode-hook 'dante-mode)
  ;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;;   (add-hook 'dante-mode-hook
  ;;             '(lambda () (flycheck-add-next-checker
  ;;                          'haskell-dante
  ;;                          '(warning . haskell-hlint))))

  ;;   )

  )

;; This is ganked from https://github.com/mbeidler/cation in an attempt to be
;; able to get ghcjsi working with emacs.  Technically it should probably be
;; inside the use-package declaration for haskell-mode, but technically it's
;; separate and I just want to try to get it working for now.
;; (use-package exec-path-from-shell
;;   :pin melpa
;;   :init
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-env "NODE_PATH"))

;; (use-package intero
;;   :ensure t
;;   :after haskell-mode
;;   :commands 'intero-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'intero-mode))

;; (use-package dante
;;   ;;:ensure t
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)

;;   ;; I'll try to get this working after I have dante itself working.
;;   ;; (add-hook 'dante-mode-hook
;;   ;;           '(lambda () (flycheck-add-next-checker 'haskell-dante
;;   ;;                                                  '(warning . haskell-hlint))))
;;   )

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

;; idris

(use-package idris-mode
  :mode "\\.idr\\'")

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

;; Lux

(require 'lux-mode)
(add-hook 'lux-mode-hook #'paredit-mode)
(add-hook 'lux-mode-hook #'rainbow-delimiters-mode)
(add-to-list 'auto-mode-alist '("\\.lux\\'" . lux-mode))

(use-package markdown-mode
  :pin melpa-stable
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config
  (setq markdown-command "/usr/local/bin/markdown"))

;; nix

(use-package nix-mode
  :ensure t
  )

;; nu

;; wtf, nu-mode?
(use-package nu-mode
  :pin melpa-stable
  :disabled t
  :mode ("\\.nu\\'" . nu-mode)
;;  :config (nu-mode 1)
  )

;; ocaml

;; (use-package tuareg
;;   :pin melpa-stable
;;   :mode "\\.ml\\'"
;;   :config
;;   (use-package utop
;;     :pin melpa-stable)
;;   (use-package merlin
;;     :pin melpa-stable))

;; purescript

(use-package purescript-mode
  :mode (("\\.purs$" . purescript-mode))

  :init
  (setq-default purescript-compile "/usr/local/bin/psc")

  :config
  (setq psc-ide-use-npm-bin t)
  (setq psc-ide-server-executable "/bin/echo")

  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)

  ;; TODO: use-package-ify ...
  (define-key purescript-mode-map (kbd "C-,") 'purescript-move-nested-left)
  (define-key purescript-mode-map (kbd "C-.") 'purescript-move-nested-right)
;;  (define-key purescript-mode-map (kbd "C-c C-c") 'purescript-compile)
  ;;(define-key purescript-cabal-mode-map (kbd "C-c C-c") 'purescript-compile)

  ;; (use-package psci
  ;;   :config
  ;;   (add-hook 'purescript-mode-hook 'inferior-psci-mode))
  )

;; (use-package psc-ide
;;   ;;:ensure t
;;   :config
;;   (add-hook 'purescript-mode-hook
;;             (lambda ()
;;               (psc-ide-mode)
;;               (company-mode)
;;               (flycheck-mode)
;;               (turn-on-purescript-indentation)
;;               ;; (customize-set-variable 'psc-ide-add-import-on-completion t)
;;               ))
;;   ;; (add-hook 'purescript-mode-hook 'psc-ide-mode)
;;   )


;; python

;; (use-package python
;;   :pin melpa-stable
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode)

;;   :config

;;   ;; Python autocompletion
;;   ;;(add-hook 'python-mode-hook 'jedi:setup)
;;   ;;(setq jedi:complete-on-dot t)

;;   ;; Fix indenting
;;   (add-hook 'python-mode-hook
;;             '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

;;   ;; In theory 'C-h S' will lookup symbols, but even after installing the python
;;   ;; info files per https://bitbucket.org/jonwaltman/pydoc-info/ it still doesn't
;;   ;; quite work for me.
;;   (use-package pydoc-info
;;     :pin melpa-stable)

;;   ;; Causing problems. Very annoying.
;;   ;;(add-hook 'python-mode-hook 'fci-mode)

;;   (use-package ipython
;;     :pin melpa-stable)

;;   ;; ===============================================
;;   ;; from http://www.reddit.com/r/emacs/comments/24l8f2/beginner_setting_up_emacs_for_python

;;   ;; would be cool but doesn't have right pythonpath when i tried it..

;;   ;; (defun my-python-f5 ()
;;   ;;   (interactive)
;;   ;;   (python-shell-send-buffer)
;;   ;;   (python-shell-switch-to-shell))

;;   ;; (eval-after-load "python"
;;   ;;   '(progn
;;   ;;      (define-key python-mode-map (kbd "<f5>") 'my-python-f5)
;;   ;;      (define-key python-mode-map (kbd "C-h f") 'python-eldoc-at-point)))
;;   ;; ===============================================

;;   (use-package virtualenvwrapper
;;     :pin melpa-stable
;;     ;; :disabled t
;;     :commands (venv-workon))

;;   (use-package cython-mode
;;     :pin melpa-stable
;;     :config (add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))))

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
;;   :mode "\\.sbt\\'")

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

;; terraform

(use-package terraform-mode
  :mode "\\.tf$"
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))


;; TOML

(use-package toml-mode
  :pin marmalade)

;; YAML

(use-package yaml-mode
  :pin melpa-stable
  :mode "\\.yml\\'")

(use-package paredit
  :pin melpa
  :diminish "par"
  :init
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'go-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'elm-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (global-set-key (kbd "C-x p") 'paredit-mode))


(global-set-key (kbd "C-c w")   'delete-trailing-whitespace)
(global-set-key (kbd "C-c l")   'linum-mode)
(global-set-key (kbd "C-c C-l") 'global-linum-mode)

;; Type greek lambda character with "M-g l"
;; Dunno why these two don't work.
;; (global-set-key (kbd "M-g c") "©")
;; (global-set-key (kbd "M-g t") "™")

(global-set-key (kbd "M-g l")   '(lambda () (interactive) (insert "λ")))
(global-set-key (kbd "M-g d")   '(lambda () (interactive) (insert "Δ")))
(global-set-key (kbd "M-g - >") '(lambda () (interactive) (insert "→")))
(global-set-key (kbd "M-g = >") '(lambda () (interactive) (insert "⇒")))
(global-set-key (kbd "M-g f")   '(lambda () (interactive) (insert "∀")))
(global-set-key (kbd "M-g E")   '(lambda () (interactive) (insert "∃")))
(global-set-key (kbd "M-g e")   '(lambda () (interactive) (insert "∈")))
(global-set-key (kbd "M-g k")   '(lambda () (interactive) (insert "©")))
(global-set-key (kbd "M-g t")   '(lambda () (interactive) (insert "™")))

(global-set-key (kbd "C-x d")
                (lambda ()
                  (interactive)
                  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))))

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
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(global-set-key (kbd "C-x r w") 'rotate-windows)
(global-set-key (kbd "C-x w r") 'rotate-windows)

(global-set-key (kbd "C-c a r") 'align-regexp)

(global-set-key (kbd "C-c r s") 'replace-string)

;; TODO: maybe 'smart or 'show-and-error ?
(setq-default org-catch-invisible-edits 'error)


;; Org Mode

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-caa" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
;; (global-set-key (kbd "C-c a a") 'org-agenda)


;; Experimental
(global-set-key (kbd "C-c r a t") 'mc/mark-all-like-this-dwim)

;; ;; TODO: Figure out how to send this to figwheel REPLs automatically:
;; (use 'figwheel-sidecar.repl-api)
;; (start-figwheel!)
;; ;; Then once that's started...
;; (cljs-repl)
(put 'erase-buffer 'disabled nil)

(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-files "elm.js")
     (add-to-list 'grep-find-ignored-directories "elm-stuff")))


(prin1-to-string grep-find-ignored-files)

(setq-default css-indent-offset 2)

;; (emacs-init-time)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-process-type (quote stack-ghci))
 '(org-agenda-files (quote ("~/Dropbox/org/")))
 '(package-selected-packages
   (quote
    (intero dumb-jump nix-mode dante cmake-mode csv-mode zencoding-mode yasnippet yaml-mode ws-trim which-key virtualenvwrapper utop use-package tuareg toml-mode terraform-mode swift-mode sly shakespeare-mode scala-mode2 sass-mode rust-mode revive restclient rainbow-mode rainbow-delimiters racket-mode quack pydoc-info psci psc-ide projectile paredit mwim multiple-cursors merlin memoize markdown-mode json-mode js2-mode ipython hydra hungry-delete helm-idris helm-git-grep helm-ag golden-ratio go-eldoc ghc geiser free-keys frame-cmds flymake-go flymake-cursor fill-column-indicator expand-region es-mode erlang elm-mode edn edit-server drag-stuff dockerfile-mode cython-mode csharp-mode coffee-mode cider beacon alchemist ace-window ace-jump-mode ac-helm)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))

(defun freenode ()
  (interactive)
  (load "~/.erc-auth.el")
  (erc :server "irc.freenode.net"
       :port 6667
       :nick erc-nick
       :password erc-password))


;; (setq compilation-scroll-output 'first-error)
