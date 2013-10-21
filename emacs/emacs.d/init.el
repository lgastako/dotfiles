(defvar *emacs-load-start* (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(menu-bar-mode 0)
(tool-bar-mode 0)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq line-number-mode t)
(setq column-number-mode t)


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

;; Marmalade Package Manager
;; http://marmalade-repo.org/about
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar my-packages '(clojure-mode
		      clojure-test-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))))
