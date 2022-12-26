(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-configuration-layers '(
                                       (org :variables
                                            org-enable-github-support t
                                            org-enable-bootstrap-support t
                                            org-enable-reveal-js-support t
                                            )
                                       bibtex
                                       (latex :variables
                                              latex-enable-auto-fill t
                                              latex-enable-folding t
                                              )
                                       html
                                       )
   dotspacemacs-additional-packages   '(
                                        (chatgpt :location (recipe
                                                            :fetcher github
                                                            :repo "joshcho/ChatGPT.el"))
                                        ;; other additional packages...
                                        )))
(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-default-font '("SourceCode Pro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-offset 2)
   ))
(defun dotspacemacs/user-init ())
(defun dotspacemacs/config ())
(defun dotspacemacs/user-config ()
  (require 'python)
  (setq chatgpt-repo-path (expand-file-name "chatgpt/" quelpa-build-dir))
  (global-set-key (kbd "C-c q") #'chatgpt-query)
  )
