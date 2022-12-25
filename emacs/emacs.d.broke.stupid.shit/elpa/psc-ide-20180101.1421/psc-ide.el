;;; psc-ide.el --- Minor mode for PureScript's psc-ide tool. -*- lexical-binding: t -*-

;; Copyright (C) 2017 The psc-ide-emacs authors
;; License: GNU General Public License version 3, or (at your option) any later version


;; Author   : Erik Post <erik@shinsetsu.nl>
;;            Dmitry Bushenko <d.bushenko@gmail.com>
;;            Christoph Hegemann
;;            Brian Sermons
;; Homepage : https://github.com/epost/psc-ide-emacs
;; Version  : 0.1.0
;; Package-Requires: ((dash "2.13.0") (dash-functional "1.2.0") (company "0.8.7") (cl-lib "0.5") (s "1.10.0") (emacs "24.4") (flycheck "0.24") (let-alist "1.0.4") (seq "1.11"))
;; Keywords : languages

;;; Commentary:

;; Emacs integration for PureScript's psc-ide tool

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imports

(require 'company)
(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'psc-ide-backported)
(require 'psc-ide-protocol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; psc-ide-mode definition

;;;###autoload
(define-minor-mode psc-ide-mode
  "psc-ide-mode definition"
  :lighter " psc-ide"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-s") 'psc-ide-server-start)
            (define-key map (kbd "C-c C-q") 'psc-ide-server-quit)
            (define-key map (kbd "C-c C-l") 'psc-ide-load-all)
            (define-key map (kbd "C-c C-S-l") 'psc-ide-load-module)
            (define-key map (kbd "C-c C-a") 'psc-ide-add-clause)
            (define-key map (kbd "C-c C-c") 'psc-ide-case-split)
            (define-key map (kbd "C-c C-i") 'psc-ide-add-import)
            (define-key map (kbd "C-c C-t") 'psc-ide-show-type)
            (define-key map (kbd "C-c C-b") 'psc-ide-rebuild)
            (define-key map (kbd "M-.") 'psc-ide-goto-definition)
            (define-key map (kbd "M-,") 'pop-tag-mark)
            map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings, override as needed.

(defgroup psc-ide nil
  "Settings for psc-ide."
  :prefix "psc-ide-"
  :group 'psc-ide)

(defcustom psc-ide-server-executable "psc-ide-server"
  "Path to the 'psc-ide-server' executable."
  :group 'psc-ide
  :type 'string)

(defcustom psc-ide-purs-executable "purs"
  "Path to the 'purs' executable."
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-use-npm-bin nil
  "Whether to use `npm bin` to determine the location of the psc ide server."
  :group 'psc-ide
  :type  'boolean)

(defcustom psc-ide-use-purs t
  "Whether to use `purs ide' to start psc ide server, if nil fallback to use old psc-ide-server."
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-port 4242
  "The port that psc-ide-server uses."
  :group 'psc-ide
  :type  'integer)

(defcustom psc-ide-source-globs '("src/**/*.purs" "bower_components/purescript-*/src/**/*.purs")
  "The source globs for your PureScript source files."
  :group 'psc-ide
  :type  'sexp)

(defcustom psc-ide-output-directory "output/"
  "Path to the output directory.
Defaults to \"output/\" and should only be changed with
.dir-locals.el to accomodate for project specific setups"
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-debug nil
  "Whether psc-ide-server should be started with the debug flag."
  :group 'psc-ide
  :type  'boolean)

(defcustom psc-ide-completion-matcher "flex"
  "The method used for completions."
  :options '("flex" "prefix")
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-add-import-on-completion "t"
  "Whether to add imports on completion."
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-add-qualification-on-completion "t"
  "Whether to automatically prepend the qualifier for completions that are imported qualified in the current module."
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-rebuild-on-save nil
  "Whether to rebuild files on save and display errors/warnings in a buffer."
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-disable-flycheck nil
  "Whether to disable flycheck syntax functionality."
  :group 'psc-ide
  :type  'boolean)

(defcustom psc-ide-editor-mode nil
  "Whether to only reload files when the editor initiates rebuilds."
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-server-extra-args nil
  "Extra arguments to pass to the purs ide executable."
  :group 'psc-ide
  :type '(repeat string))

(defconst psc-ide-import-regex
  (rx (and line-start "import" (1+ space)
           (group (and (1+ (any word "."))))
           (opt (1+ space) "hiding")
           (opt (1+ space) "(" (group (0+ not-newline)) ")")
           (opt (1+ space) "as" (1+ space) (group (and (1+ word)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive.

(add-hook 'after-save-hook
          (lambda ()
            (set 'psc-ide-buffer-import-list
                 (psc-ide-parse-imports-in-buffer)))
          nil t)

(defun psc-ide-rebuild-on-save-hook()
  "Rebuilds the current module on save."
  (when (eq major-mode 'purescript-mode)
    (psc-ide-rebuild)))

(when psc-ide-rebuild-on-save
  (add-hook 'after-save-hook 'psc-ide-rebuild-on-save-hook))

(defun psc-ide-init ()
  "Initialization for psc-ide-mode."
  (interactive)
  (add-function :before-until (local 'eldoc-documentation-function)
                #'psc-ide-show-type-eldoc)
  (set (make-local-variable 'psc-ide-buffer-import-list)
       (psc-ide-parse-imports-in-buffer)))

(with-eval-after-load 'flycheck
  (when (not psc-ide-disable-flycheck)
    (require 'psc-ide-flycheck)
    (psc-ide-flycheck-setup)))

(defun company-psc-ide-backend (command &optional arg &rest ignored)
  "The psc-ide backend for 'company-mode'."
  (interactive (list 'interactive))

  (when (derived-mode-p 'purescript-mode)
    (cl-case command
      (interactive (company-begin-backend 'company-psc-ide-backend))

      (init (psc-ide-init))

      (prefix (when (and (eq major-mode 'purescript-mode)
                         (not (company-in-string-or-comment)))
                (let ((symbol (company-grab-symbol)))
                  (if symbol
                      ;; We strip of the qualifier so that it doesn't get
                      ;; overwritten when completing.
                      (if (s-contains-p "." symbol)
                          (cons (car (last (s-split "\\." symbol))) t)
                        symbol)
                    'stop))))

      (candidates (psc-ide-company-fetcher arg company--manual-action))

      (doc-buffer (company-doc-buffer (get-text-property 0 :documentation arg)))

      (sorted t)

      (annotation (psc-ide-annotation arg))

      (meta (psc-ide-string-fontified (get-text-property 0 :type arg)))

      (post-completion
       (unless (or
                ;; Don't add an import when the option to do so is disabled
                (not psc-ide-add-import-on-completion)
                ;; or when a qualified identifier was completed
                (or (get-text-property 0 :qualifier arg) (s-contains-p "." (company-grab-symbol)))
                ;; Don't attempt to import Prim members
                (string= (get-text-property 0 :module arg) "Prim"))
         (psc-ide-add-import-impl arg (vector
                                       (psc-ide-filter-modules
                                        (list (get-text-property 0 :module arg))))))))))

(defun psc-ide-server-start (dir-name)
  "Start 'psc-ide-server' in DIR-NAME and load all modules."
  (interactive (list (read-directory-name "Project root? "
                                          (psc-ide-suggest-project-dir))))
  (psc-ide-server-start-impl dir-name)
  ;; After 1 second we send a load all command
  (run-at-time "1 sec" nil 'psc-ide-load-all))

(defun psc-ide-server-quit ()
  "Quit 'psc-ide-server'."
  (interactive)
  (psc-ide-send-sync psc-ide-command-quit))

(defun psc-ide-load-module (module-name)
  "Load module with MODULE-NAME."
  (interactive (list (read-string "Module: " (psc-ide-get-module-name))))
  (psc-ide-load-module-impl module-name))

(defun psc-ide-load-all ()
  "Load all modules in the current project."
  (interactive)
  (psc-ide-send psc-ide-command-load-all
                (-compose 'message 'psc-ide-unwrap-result)))

(defun psc-ide-show-type (expand)
  "Show type of the symbol under cursor and EXPAND its type synonyms."
  (interactive "P")
  (psc-ide-show-type-impl (psc-ide-ident-at-point) t expand))

(defun psc-ide-goto-definition ()
  "Go to definition of the symbol under cursor."
  (interactive)
  (let ((ident (psc-ide-ident-at-point)))
    (psc-ide-goto-definition-impl ident)))

(defun psc-ide-case-split (type)
  "Case Split on identifier under cursor while specifying TYPE."
  (interactive "sType: ")
  (let ((new-lines (psc-ide-case-split-impl type)))
    (beginning-of-line) (kill-line) ;; clears the current line
    (insert (mapconcat 'identity new-lines "\n"))))

(defun psc-ide-add-clause ()
  "Add clause on identifier under cursor."
  (interactive)
  (let ((new-lines (psc-ide-add-clause-impl)))
    (beginning-of-line) (kill-line) ;; clears the current line
    (insert (mapconcat 'identity new-lines "\n"))))

(defun psc-ide-add-import ()
  "Add an import for the symbol under the cursor."
  (interactive)
  (-if-let (splitted (psc-ide-split-qualifier (psc-ide-ident-at-point)))
      (let-alist splitted
        (psc-ide-add-import-qualified-impl .identifier .qualifier))
    (psc-ide-add-import-impl (psc-ide-ident-at-point))))

(defun psc-ide-rebuild ()
  "Rebuild the current module."
  (interactive)
  (save-buffer)
  (psc-ide-send (psc-ide-command-rebuild) 'psc-ide-rebuild-handler))

(defun psc-ide-rebuild-handler (response)
  "Handle a rebuild RESPONSE.
Either displays errors/warnings inside the *psc-ide-rebuild*
buffer, or closes it if there were none."
  (let ((is-success (string= "success" (cdr (assoc 'resultType response))))
        (result (cdr (assoc 'result response))))
    (if (not is-success)
        (psc-ide-display-rebuild-message "Error" (aref result 0))
      (if (<= (length result) 0)
          ;; If there are no warnings we close the rebuild buffer and print "OK"
          (progn
            (delete-windows-on (get-buffer-create "*psc-ide-rebuild*"))
            (message "OK"))
        (psc-ide-display-rebuild-message "Warning" (aref result 0))))))

(defun psc-ide-display-rebuild-message (error-type raw-message)
  "Take a parsed JSON error/warning and displays it in the rebuild buffer.
ERROR-TYPE is either \"error\" or \"warning\" and gets displayed with the RAW-MESSAGE."
  (let ((msg (concat error-type ": " (psc-ide-pretty-json-error raw-message))))
    (progn
      (with-current-buffer (get-buffer-create "*psc-ide-rebuild*")
        (compilation-mode)
        (read-only-mode -1)
        (erase-buffer)
        (insert msg)
        (read-only-mode 1))
      (display-buffer "*psc-ide-rebuild*")
      (set-window-point (get-buffer-window "*psc-ide-rebuild*") (point-min)))))

(defun psc-ide-pretty-json-error (purs-error)
  "Formats a parsed PURS-ERROR for display inside a compilation buffer."
  (let-alist purs-error
    (if (not .position)
        .message
      (concat .filename
              ":" (number-to-string .position.startLine)
              ":" (number-to-string .position.startColumn)
              ":" "\n" .message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Non-interactive.

(defun psc-ide-show-type-eldoc ()
  "Show type of the symbol under cursor, but be quiet about failures."
  (psc-ide-show-type-impl (psc-ide-ident-at-point)))

(defun psc-ide-case-split-impl (type)
  "Case Split on identifier of TYPE under cursor."
  (let ((reg (psc-ide-ident-pos-at-point)))
    (psc-ide-unwrap-result (psc-ide-send-sync
                            (psc-ide-command-case-split
                             (substring (thing-at-point 'line t) 0 -1)
                             (save-excursion (goto-char (car reg)) (current-column))
                             (save-excursion (goto-char (cdr reg)) (current-column))
                             type)))))

(defun psc-ide-add-clause-impl ()
  "Add clause on identifier under cursor."
  (psc-ide-unwrap-result (psc-ide-send-sync
                          (psc-ide-command-add-clause
                           (substring (thing-at-point 'line t) 0 -1) nil))))

(defun psc-ide-get-module-name ()
  "Return the qualified name of the module in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^module +\\([A-Z][A-Za-z0-9.]*\\)" nil t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun psc-ide-extract-import-from-match-data (&optional string)
  "Helper to extract the relevant info when using the `psc-ide-import-regex'.
STRING is for use when the search used was with `string-match'."
  (let (result)
    (push `(module . ,(match-string-no-properties 1 string)) result)
    (push `(qualifier . ,(match-string-no-properties 3 string)) result)
    result))

(defun psc-ide-parse-imports-in-buffer (&optional buffer)
  "Parse the list of imports for the current purescript BUFFER."
  (let ((module-name (psc-ide-get-module-name))
        (matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when module-name
              (push `((module . ,module-name)) matches))
            (while (search-forward-regexp psc-ide-import-regex nil t 1)
              (push (psc-ide-extract-import-from-match-data) matches))))))
    matches))


(defun psc-ide-ask-project-dir ()
  "Ask psc-ide server for the project dir."
  (interactive)
  (psc-ide-send psc-ide-command-cwd
                (-compose 'message 'psc-ide-unwrap-result)))

(defun psc-ide-server-start-impl (dir-name)
  "Start psc-ide server in DIR-NAME."
  (apply 'start-process `("*psc-ide-server*" "*psc-ide-server*"
                          ,@(psc-ide-server-command dir-name))))

(defun psc-ide-server-command (dir-name)
  "Build a shell command to start `purs ide` in directory DIR-NAME.
Tries to find the purs executable and builds up the command by
appending eventual options. Returns a list that can be expanded
and passed to `start-process`."
  (let* ((executable-name (psc-ide-executable-name))
         (npm-bin-path (if psc-ide-use-npm-bin
                           (psc-ide-npm-bin-server-executable executable-name)
                         nil))
         (path (or npm-bin-path (executable-find executable-name)))
         (cmd (if psc-ide-use-purs
                  `(,path "ide" "server")
                `(,path)))
         (port (number-to-string psc-ide-port))
         (directory (expand-file-name dir-name))
         (debug-flags (when psc-ide-debug (if psc-ide-use-purs
                                              '("--log-level" "debug")
                                            '("--debug"))))
         (editor-mode (when psc-ide-editor-mode '("--editor-mode"))))
    (if path
        (remove nil `(,@cmd "-p" ,port "-d" ,directory
                            "--output-directory" ,psc-ide-output-directory
                            ,@editor-mode
                            ,@debug-flags ,@psc-ide-server-extra-args
                            ,@psc-ide-source-globs))
      (error (s-join " " '("Couldn't locate psc ide executable. You"
                           "could either customize the psc-ide-purs-executable"
                           " or psc-ide-server-executable if psc-ide-use-purs is nil,"
                           " or set the psc-ide-use-npm-bin variable to"
                           " true, or put the executable on your path."))))))
(defun psc-ide-executable-name ()
  "Find ide executable name."
  (if psc-ide-use-purs
      psc-ide-purs-executable
    psc-ide-server-executable))

(defun psc-ide-npm-bin-server-executable (cmd)
  "Find psc-ide server binary CMD of current project by invoking `npm bin`."
  (let* ((npm-bin (s-trim-right (shell-command-to-string "npm bin")))
         (server (expand-file-name cmd npm-bin)))
    (if (and server (file-executable-p server)) server nil)))

(defun psc-ide-server-version ()
  "Return the version of the found psc-ide-server executable."
  (let ((path (executable-find (psc-ide-executable-name))))
    (s-chomp (shell-command-to-string (s-concat path " --version")))))

(defun psc-ide-load-module-impl (module-name)
  "Load a PureScript module with MODULE-NAME and its dependencies."
  (psc-ide-unwrap-result
   (psc-ide-send-sync (psc-ide-command-load
                       [] (list module-name)))))

(defun psc-ide-add-import-impl (identifier &optional filters)
  "Invoke the addImport command for IDENTIFIER with the given FILTERS."
  (let* ((tmp-file (make-temp-file "psc-ide-add-import"))
         (result (progn
                   (write-region (point-min) (point-max) tmp-file)
                   (psc-ide-unwrap-result
                    (psc-ide-send-sync (psc-ide-command-add-import identifier filters tmp-file tmp-file))))))
    (if (not (stringp result))
        (let ((selection
               (completing-read "Which Module to import from: "
                                (-map (lambda (x)
                                        (cdr (assoc 'module x))) result))))
          (psc-ide-add-import-impl identifier (vector (psc-ide-filter-modules (vector selection)))))
      (progn (message "Added import for %s" identifier)
             (save-restriction
               (widen)
               ;; command successful, insert file with replacement to preserve
               ;; markers.
               (insert-file-contents tmp-file nil nil nil t))))
    (delete-file tmp-file)))

(defun psc-ide-add-import-qualified-impl (identifier qualifier)
  "Add a qualified import for the given IDENTIFIER and QUALIFIER."
  (let* ((completions
          (psc-ide-unwrap-result
           (psc-ide-send-sync
            (psc-ide-command-show-type (vector) identifier))))
         (module
          (pcase (length completions)
            (`0 (error "Couldn't find a module for %s" identifier))
            (`1 (cdr (assoc 'module (aref completions 0))))
            (_ (completing-read "Which Module: "
                                (seq-map (lambda (x) (let-alist x .module)) completions))))))
    (unless (string= (psc-ide-qualifier-for-module module) qualifier)
      (save-buffer)
      (psc-ide-send-sync (psc-ide-command-add-qualified-import module qualifier))
      (revert-buffer nil t))))

(defun psc-ide-company-fetcher (ignored &optional manual)
  "Create an asynchronous company fetcher.
Grabs the symbol at point at creates an asynchronous
completer. We ignore the prefix we get from company, because it
doesn't contain eventual qualifiers."
  (let ((prefix (company-grab-symbol)))
  `(:async . ,(-partial 'psc-ide-complete-async prefix manual))))

(defun psc-ide-complete-async (prefix manual callback)
  "Sends a completion command for PREFIX to purs ide.
If MANUAL is set, do not filter with the currently imported modules.
CALLBACK receives the asynchronously retrieved completions."
  (let ((command (psc-ide-build-completion-command prefix manual))
        (handler (-partial 'psc-ide-handle-completionresponse callback)))
    (psc-ide-send command handler)))

(defun psc-ide-split-qualifier (s)
  "Split S into (qualifier . identifier), or return nil if there is no qualifier."
  (let* ((splitted (s-split "\\." s))
         (qualifier (s-join "." (butlast splitted)))
         (identifier (-last-item splitted)))
    (when (and identifier (not (string= "" qualifier)) (s-uppercase? (substring qualifier 0 1)))
      `((identifier . ,identifier)
        (qualifier . ,qualifier)))))

(defun psc-ide-build-completion-command (search manual)
  "Construct a completion command from the given SEARCH.

If MANUAL is set, ignore the currently imported modules.

The cases we have to cover:
1. List.fil      <- filter by prefix and List module
2. fil| + manual <- don't filter at all
3. fil|          <- filter by prefix and imported modules"
  (let* ((components (s-split "\\." search))
         (prefix (car (last components)))
         (qualifier (s-join "." (butlast components))))
    (if (not (s-blank? qualifier))
        ;; 1. List.fil <- filter by prefix and List module
        (psc-ide-qualified-completion-command prefix qualifier)
      (if manual
          ;; 2. fil| + manual <- don't filter at all
          (psc-ide-command-complete
           (vector (psc-ide-filter-prefix prefix))
           nil
           (psc-ide-get-module-name))
        ;; 3. fil| <- filter by prefix and imported modules"
        (psc-ide-command-complete
         (vector (psc-ide-filter-prefix prefix)
                 (psc-ide-filter-modules (cons "Prim" (psc-ide-all-imported-modules))))
         nil
         (psc-ide-get-module-name))))))

(defun psc-ide-qualified-completion-command (prefix qualifier)
  "Build a completion command for a PREFIX with QUALIFIER."
  (let ((modules (psc-ide-modules-for-qualifier qualifier)))
    (psc-ide-command-complete
     (vector (psc-ide-filter-prefix prefix)
             (psc-ide-filter-modules (vconcat modules)))
     nil
     (psc-ide-get-module-name))))

(defun psc-ide-all-imported-modules ()
  "Retrieve all imported modules for a buffer."
  (-map (lambda (import) (cdr (assoc 'module import)))
        (psc-ide-parse-imports-in-buffer)))

(defun psc-ide-modules-for-qualifier (qualifier)
  "Search the current module's imports for modules that are qualified as QUALIFIER."
  (let ((imports (psc-ide-parse-imports-in-buffer)))
    (-keep (lambda (import)
             (when (equal qualifier (cdr (assoc 'qualifier import)))
               (cdr (assoc 'module import)))) imports)))

(defun psc-ide-qualifier-for-module (module &optional parsed-imports)
  "Search the current module's imports for MODULE and return its qualifier.
Return nil if the module is not imported qualified. Does not
reparse if PARSED-IMPORTS is passed."
  (let ((imports (or parsed-imports (psc-ide-parse-imports-in-buffer))))
    (-first-item
     (-keep (lambda (import)
              (when (equal module (cdr (assoc 'module import)))
                (cdr (assoc 'qualifier import)))) imports))))

(defun psc-ide-handle-completionresponse (callback response)
  "Handle a completion response from psc-ide.
Accepts a CALLBACK and a completion RESPONSE from psc-ide,
processes the response into a format suitable for company and
passes it into the callback"
  (let* ((result (psc-ide-unwrap-result response))
         (completions (-map (-partial 'psc-ide-annotate-completion (psc-ide-parse-imports-in-buffer)) result)))
    (funcall callback completions)))

(defun psc-ide-annotate-completion (parsed-imports completion)
  "Annotate a completion from psc-ide with `text-properties'.
PARSED-IMPORTS are used to annotate the COMPLETION with qualifiers."
  (let-alist completion
    (let* ((qualifier (psc-ide-qualifier-for-module .module parsed-imports))
           (identifier  (if (and psc-ide-add-qualification-on-completion
                                 qualifier
                                 ;; Don't add a qualifier if we're already
                                 ;; completing a qualified prefix
                                 (not (s-contains-p "." (company-grab-symbol))))
                            (format "%s.%s" qualifier .identifier)
                          .identifier)))

      (add-text-properties 0 1 (list :type .type
                                     :module .module
                                     :qualifier qualifier
                                     :documentation .documentation) identifier)
      ;; add-text-properties is sideeffecting and doesn't return the modified
      ;; string, so we need to explicitly return the identifier from here
      identifier)))

(defun psc-ide-goto-definition-impl (search)
  "Asks for the definition location of SEARCH and jumps to it."
  (let* ((resp (psc-ide-send-sync
                (psc-ide-build-type-command search)))
         (result (psc-ide-unwrap-result resp)))
    (when (not (zerop (length result)))
      (let* ((completion (aref result 0))
             (position (cdr (assoc 'definedAt completion))))
        (if position
            (let* ((file (cdr (assoc 'name position)))
                   (start (cdr (assoc 'start position)))
                   (line (aref start 0))
                   (column (aref start 1)))
              (if (fboundp 'xref-push-marker-stack) ;; Emacs 25
                  (xref-push-marker-stack)
                (with-no-warnings
                  (require 'etags)
                  (ring-insert find-tag-marker-ring (point-marker))))
              (find-file (expand-file-name file))
              (goto-char (point-min))
              (forward-line (1- line))
              (forward-char (1- column)))
          (message (format "No position information for %s" search)))))))

(defun psc-ide-show-type-impl (search &optional warn expand)
  "Print a message that describes the type of SEARCH.
If the type of SEARCH is not found it prints a warning depending
on whether WARN is true. Optionally EXPANDs type synonyms."
  (let ((handler
         (lambda (resp)
           (let ((result (psc-ide-unwrap-result resp)))
             (if (not (zerop (length result)))
               (let-alist (aref result 0)
                 (message (psc-ide-string-fontified
                           (format "%s.%s ∷\n  %s"
                                   .module
                                   .identifier
                                   (if expand .expandedType .type)))))
               (when warn (message "Know nothing about type of `%s'." search)))))))
    (psc-ide-send (psc-ide-build-type-command search) handler)))

(defun psc-ide-build-type-command (search)
  "Builds a type command from SEARCH."
  (let* ((components (s-split "\\." search))
         (ident (car (last components)))
         (qualifier (s-join "." (butlast components))))
    (if (not (s-blank? qualifier))
      (psc-ide-qualified-type-command ident qualifier)
    (psc-ide-command-show-type
     (vector (psc-ide-filter-modules
              (cons "Prim" (psc-ide-all-imported-modules))))
     search
     (psc-ide-get-module-name)))))

(defun psc-ide-qualified-type-command (ident qualifier)
  "Build a type command for an IDENT with QUALIFIER."
  (let ((modules (psc-ide-modules-for-qualifier qualifier)))
        (psc-ide-command-show-type
         (vector (psc-ide-filter-modules (vconcat modules)))
         ident
         (psc-ide-get-module-name))))

(defun psc-ide-annotation (s)
  "Extract the module annotation from S."
  (format " (%s)" (get-text-property 0 :module s)))

(defun psc-ide-suggest-project-dir ()
  "Suggest a project directory to start the ide server in."
  (if (and (fboundp 'projectile-project-root) (projectile-project-p))
      (projectile-project-root)
    (file-name-directory (buffer-file-name))))

(defun psc-ide-string-fontified (string)
  "Take a STRING and return it with syntax highlighting."
  (if (fboundp 'turn-on-purescript-font-lock)
      (with-temp-buffer
        (turn-on-purescript-font-lock)
        (insert string)
        (font-lock-ensure)
        (buffer-string))
    string))

(setq company-tooltip-align-annotations t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities

(add-to-list 'company-backends 'company-psc-ide-backend)

(provide 'psc-ide)

;;; psc-ide.el ends here
