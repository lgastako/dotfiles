;;; init.el --- John's Emacs Configuration

;; Store the start time for later reporting
(defvar *emacs-load-start* (current-time))
;; Thanks anarcat!
(defun anarcat/time-to-ms (time)
  (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car (cdr (cdr time)))))
(defun anarcat/display-timing ()
  (message ".emacs loaded in %fms" (/ (- (anarcat/time-to-ms (current-time)) (anarcat/time-to-ms *emacs-load-start*)) 1000000.0)))
(add-hook 'after-init-hook 'anarcat/display-timing t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Answer with 'y' or 'n' instead of having to type of 'yes' or 'no'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight the current line...
(global-hl-line-mode t)
;; ...darker
(set-face-background 'hl-line "grey9")
;; for a list of colors: http://raebear.net/comp/emacscolors.html

;; We load this early to maximize the niceness of the editing environment if
;; init.el explodes.

;; Pretty Font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil
                :stipple nil
                :inverse-video nil
                :box nil
                :strike-through nil
                :overline nil
                :underline nil
                :slant normal
                :weight normal
                :height 235
                :width normal
                :foundry "apple"
                :family "Monaco")))))

(load-theme 'deeper-blue)

;; We do this right after the theme is loaded to minimize the time it looks
;; wonky.
;; (if (> (x-display-pixel-width) 2000)
;;     (set-face-attribute 'default nil :height 235)
;;   (set-face-attribute 'default nil :height 172))
(set-face-attribute 'default nil :height 172)
;; (set-face-attribute 'default nil :height 200)
;;(set-face-attribute 'default nil :height 256)
;;(set-face-attribute 'default nil :height 240)

;; Helper for more natural path additions
(defun add-to-load-path-list (fn)
  (add-to-list 'load-path (expand-file-name fn)))

;; Individual .el files here
(add-to-load-path-list "~/.emacs.d/elisp")

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; Marmalade Package Manager
;; http://marmalade-repo.org/about

(require' package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; have use-package install missing packages automatically
(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Declutter the UI by hiding the menus
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Maximize window on startup
(load "frame-cmds.el")
(maximize-frame-vertically)
(maximize-frame-horizontally)

;; Highlight matching delimiters
(show-paren-mode 1)

;; Prevent insertion of tabs for spaces
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

;; Show (line, col) in modeline
(setq-default line-number-mode t)
(setq-default column-number-mode t)

;; Highlight trailing whitespace in red
(setq-default show-trailing-whitespace t)

;; Set the print margin
(setq-default fill-column 79)

(setq auto-save-visited-file-name nil)
;; (setq auto-save-visited-file-name t)
;; (setq auto-save-interval 5)  ; keystrokes
;; (setq auto-save-timeout 5)   ; seconds
;; (setq cider-auto-select-error-buffer t)

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

(use-package ido
  :config (ido-mode t))

;; (use-package easymenu)

(use-package rainbow-delimiters
  ;; For all programming modes:
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package theme-park-mode)

(use-package flymake-cursor)

(use-package ws-trim
  :init   (setq ws-trim-global-modes t)
  :config (global-ws-trim-mode t))

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package fill-column-indicator)

;; Better uniqification of buffer names
;; doesn't work with use-package for some reason
(require 'uniquify)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c m") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Snippets
(use-package yasnippet
  :load-path "~/.emacs..d/plugins/yasnippet"
  :config
  (setq yas/root-directory "~/.emacs.d/snippets")
  (yas-load-directory yas/root-directory)
  (yas-global-mode 1))

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x p") 'paredit-mode)

(global-set-key (kbd "C-c l")   'linum-mode)
(global-set-key (kbd "C-c C-l") 'global-linum-mode)

(defun indent-all ()
  (interactive)
  (indent-region 0 (buffer-size)))

(global-set-key (kbd "C-c f") 'indent-all)

;;http://www.emacswiki.org/emacs/BackupDirectory
(setq backup-by-copying t       ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))     ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)        ; use versioned backups

(defvar my-packages '(cl
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; http://www.emacswiki.org/emacs/PareditCheatsheet
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 ;; '(helm-google-search-function (quote helm-google-api-search))
 '(quack-programs (quote ("/Users/john/local/bin/racket" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi"
                          "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme" "mzschme" "racket"
                          "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))
 '(tpm-tagged nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;
;; Haven't properly merged these into the appropriate spots above.

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key "\C-y" 'yank-and-indent)

(setenv "PATH" (concat (getenv "PATH") ":$HOME/bin"))
(setq sql-postgres-program "/usr/local/bin/psql")
;;(setq sql-port ...) -- seems to not be a good idea
;; this seems to be the way: sigh
;;(setq sql-postgres-options (list "-p 5492"))

;;failed to compile
;;(load-file "/Users/john/.emacs.d/elisp/ProofGeneral/generic/proof-site.el")

(use-package dockerfile-mode
  :init (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package helm
  :config

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

  ;; My muscle memory is set to "C-x-b" for selecting buffers, so lets change that to helm:
  (global-set-key (kbd "C-x C-b") 'list-buffers)
  (global-set-key (kbd "C-x b") 'helm-mini)

  (helm-mode 1)

  (use-package ac-helm
    :bind (("C-c g"   . helm-git-grep)
           ("C-c t"   . helm-git-grep-at-point)
           ("C-x C-f" . helm-find-files)
           ("C-c M-i" . heml-swoop))
    :config
    (global-set-key (kbd "C-;") 'ac-complete-with-helm)
    (define-key ac-complete-mode-map (kbd "C-;") 'ac-complete-with-helm)
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

;; Bind a key to edit my init.el
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file "~/dotfiles/emacs/emacs.d/init.el")))

(use-package ace-jump-mode
  ;; Enable Ace Jump mode
  ;;   'C-u C-c SPC <char>' to jump to a specific char
  ;;   'C-c SPC <char>' to jump to a specific first-char
  :bind (("C-c SPC" . ace-jump-mode)
         ("C-c C-SPC" . ace-jump-mode))
  :config
  (defvar ace-jump-mode-submode-list
    '(ace-jump-char-mode
      ace-jump-word-mode
      ace-jump-line-mode)))

(use-package winner
  ;; Winner mode
  ;;   'C-c left' and 'C-c right' to undo/redo changes to window settings
  :init (winner-mode))

(use-package drag-stuff
  ;; 'M-N' / 'M-P' to move lines or selected groups of lines up/down
  ;; 'M-<left>' / 'M-<right>' to move words or selected regions
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
         ("M-P" . drag-stuff-up)))

(use-package company
  :bind ("C-." . company-complete)
  :init (global-company-mode 1)
  :config (bind-keys :map company-active-map
                     ("C-n"   . company-select-next)
                     ("C-p"   . company-select-previous)
                     ("C-d"   . company-show-doc-buffer)
                     ("<tab>" . company-complete)))

(use-package expand-region
  :bind (("C-@" . er/expand-region)
         ("C-=" . er/expand-region)
         ("M-3" . er/expand-region))
  :init (delete-selection-mode))

;; (use-package hungry-delete
;;   :init (global-hungry-delete-mode))

(use-package beacon
  :diminish beacon-mode
  :init
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#cccc00"))

(add-to-list 'auto-mode-alist '("\\.visualforcepage$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vfc$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.apexclass$" . js-mode))

;; Something like this should work, but it doesn't, so for now...
;; ;; Load all the individual language configs
;; (require 'load-directory)
;; (load-directory "~/.emacs.d/languages")
;; ... we do it manually:
(load-file "~/.emacs.d/languages/clojure.el")
(load-file "~/.emacs.d/languages/coffee.el")
(load-file "~/.emacs.d/languages/go.el")
(load-file "~/.emacs.d/languages/haml.el")
(load-file "~/.emacs.d/languages/haskell.el")
(load-file "~/.emacs.d/languages/html.el")
(load-file "~/.emacs.d/languages/markdown.el")
(load-file "~/.emacs.d/languages/python.el")
(load-file "~/.emacs.d/languages/ruby.el")
(load-file "~/.emacs.d/languages/rust.el")
(load-file "~/.emacs.d/languages/salesforce.el")
(load-file "~/.emacs.d/languages/sass.el")
(load-file "~/.emacs.d/languages/scheme.el")
(load-file "~/.emacs.d/languages/yaml.el")

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq erc-track-enable-keybindings nil)
