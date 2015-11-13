;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("g" "import logging\n\nlogger = logging.getLogger(__name__)\n$0\n" "logging" nil nil nil nil nil nil)
                       ("i" "import ipdb; ipdb.set_trace()" "ipdb" nil nil nil nil nil nil)
                       ("ifm" "if __name__ == \"__main__\":\n   main()" "if-main" nil nil nil nil nil nil)
                       ("l" "logger.debug(\"$0\")" "logger.debug" nil nil nil nil nil nil)
                       ("m" "\"\"\"\n${1:program_name.py}.\n\nUsage:\n$1 foo <bar>...\n$1 -h | --help\n$1 --version\n\nOptions:\n-h --help     Show this screen.\n--version     Show version.\n\"\"\"\nfrom docopt import docopt\n\ndef main():\n    args = docopt(__doc__, version=\"$1 0.1\")\n    $0\n    print(args)\n\nif __name__ == \"__main__\":\n    main()\n" "main" nil nil nil nil nil nil)
                       ("super" "super(`(replace-regexp-in-string \"\\\\([.]\\\\)[^.]+$\" \", self).\" (python-info-current-defun) nil nil 1)`($1)\n$0" "super" nil
                        ("object oriented")
                        nil nil nil nil)
                       ("try" "try:\n    $1\nexcept ${2:Exception}:\n    $0" "try" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Dec  3 23:27:39 2014
