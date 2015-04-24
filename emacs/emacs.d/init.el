;; Store the start time for later reporting
(defvar *emacs-load-start* (current-time))

;; Answer with 'y' or 'n' instead of having to type of 'yes' or 'no'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight the current line...
(global-hl-line-mode t)
;; ...in black
;; (set-face-background 'hl-line "black")
;; ..ish
(set-face-background 'hl-line "grey9")
;; for a list of colors: http://raebear.net/comp/emacscolors.html

;; Helper for more natural path additions
(defun add-to-load-path-list (fn)
  (add-to-list 'load-path (expand-file-name fn)))

;; Individual .el files here
(add-to-load-path-list "~/.emacs.d/elisp")

;; Directory-based plugins here
(add-to-load-path-list "~/.emacs.d/plugins/yasnippet")

;; Marmalade Package Manager
;; http://marmalade-repo.org/about
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; bah humbug - too slow, doesn't pay for itself
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

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
(set-face-attribute 'default nil :height 200)
;;(set-face-attribute 'default nil :height 256)
;;(set-face-attribute 'default nil :height 240)

;; Maximize window on startup
(load "frame-cmds.el")
(maximize-frame-vertically)
(maximize-frame-horizontally)

;; Declutter the UI by hiding the menus
(menu-bar-mode 0)
(tool-bar-mode 0)

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

(require 'ido)
(ido-mode t)

;; (require 'easymenu)

(require 'rainbow-delimiters)

;; For all programming modes:
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'theme-park-mode)

(require 'flymake-cursor)

(require 'ws-trim)
(global-ws-trim-mode t)
(setq ws-trim-global-modes t)

(require 'edit-server)
(edit-server-start)

(require 'fill-column-indicator)

;; Better uniqification of buffer names
(require 'uniquify)

(require 'multiple-cursors)

(global-set-key (kbd "C-c m") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Snippets
(require 'yasnippet)
(yas--initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas-load-directory yas/root-directory)
(yas-global-mode 1)

(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x p") 'paredit-mode)

(defun indent-all ()
  (interactive)
  (indent-region 0 (buffer-size)))

(global-set-key (kbd "C-c f") 'indent-all)

;;(global-set-key (kbd "C-c t") 'nrepl-make-repl-connection-default)

;;http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t       ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
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
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(helm-google-search-function (quote helm-google-api-search))
 '(quack-programs (quote ("/Users/john/local/bin/racket" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mzscheme" "mzschme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
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

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

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
;; https://github.com/emacs-helm/helm-cmd-t

;; Bind a key to edit my init.el
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file "~/dotfiles/emacs/emacs.d/init.el")))

;; (require 'helm-git-grep) ;; Not necessary if installed by package.el
(global-set-key (kbd "C-c t") 'helm-git-grep-at-point)
(global-set-key (kbd "C-c g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

(global-set-key (kbd "C-c M-i") 'helm-swoop)

(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c C-l") 'global-linum-mode)

;; Enable Ace Jump mode
;;   'C-u C-c SPC <char>' to jump to a specific char
;;   'C-c SPC <char>' to jump to a specific first-char
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)

(defvar ace-jump-mode-submode-list
  '(ace-jump-char-mode
    ace-jump-word-mode
    ace-jump-line-mode))

;; Winner mode
;;   'C-c left' and 'C-c right' to undo/redo changes to window settings
(when (fboundp 'winner-mode)
  (winner-mode 1))

(add-to-list 'auto-mode-alist '("\\.visualforcepage$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vfc$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.apexclass$" . js-mode))

(require 'edit-server)
(edit-server-start)

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

(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; /sudo::/etc/hosts doesn't work for some reason.

;; (defadvice helm-find-files (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo::" buffer-file-name))))

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq erc-track-enable-keybindings nil)
