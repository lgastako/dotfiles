;; Bind a key to edit this file
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file "~/dotfiles/emacs/emacs.d/init.el")))

;; Prevent me from accidentally minimizing emacs.
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-z") nil)

;; For local customizations and stuff not in a repo
(add-to-list 'load-path "~/dotfiles/emacs/emacs.d/lisp")

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

;; Declutter the UI by hiding the menus
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0))

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

;; Highlight matching delimiters
(show-paren-mode 1)

;; Never tabs
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

;; Maintain case in dynamic expansions
(setq-default dabbrev-case-fold-search nil)

;; Prevent insertion of tabs for spaces
(setq-default indent-tabs-mode nil)

;; Show (line, col) in modeline
(setq-default line-number-mode t
              column-number-mode t)


;; Set the print margin
(setq-default fill-column 79)

(setq auto-save-visited-file-name nil)

;; Use shift-arrow keys to move between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Don't use outdated compiled elisp
(setq load-prefer-newer t)

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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq mode-name "Eλ")))

(use-package frame-cmds
  :ensure t
  :pin melpa-stable
  :bind (("C-c f m"   . maximize-frame)
         ("C-c f r"   . restore-frame)
         ("C-c f o"   . other-window-or-frame)
         ("<M-up>"    . move-frame-up)
         ("<M-down>"  . move-frame-down)
         ("<M-left>"  . move-frame-left)
         ("<M-right>" . move-frame-right))
  :config
  ;; Maximize window on startup
  (maximize-frame))
