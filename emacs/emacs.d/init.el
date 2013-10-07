(defvar *emacs-load-start* (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp"))
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Font
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "apple" :family "Monaco")))))
(load-theme 'deeper-blue)

;; Maximize Window
(load "frame-cmds.el")
(maximize-frame-vertically)
(maximize-frame-horizontally)

;; Marmalade Package Manager
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
