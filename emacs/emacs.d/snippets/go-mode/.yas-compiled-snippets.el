;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("m" "package main\n\nimport (\n	\"fmt\"\n\n	docopt \"github.com/docopt/docopt-go\"\n)\n$0\nfunc main() {\n	usage := \\`${1:REPLACE_ME}.\n\nUsage:\n  $1 ${2:command} ${3:<arg>...}\n  $1 -h | --help\n  $1 --version\n\nOptions:\n  -h --help     Show this screen.\n  --version     Show version.\\`\n\n	arguments, _ := docopt.Parse(usage, nil, true, \"$1 0.1\", false)\n	fmt.Println(arguments)\n}\n" "main" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Dec  3 23:27:39 2014
