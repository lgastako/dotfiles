;;; Compiled snippets and support files for `sql-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sql-mode
                     '(("cas" "ON UPDATE CASCADE\nON DELETE CASCADE" "cascade" nil nil nil nil nil nil)
                       ("for" "FOREIGN KEY (${1})\n    REFERENCES ${2:table_name} (${2} _ ${3:slug})\n        ON UPDATE ${4:CASCADE}\n        ON DELETE ${5:RESTRICT}," "foreign_key" nil nil nil nil nil nil)
                       ("odc" "ON DELETE CASCADE" "odc" nil nil nil nil nil nil)
                       ("ouc" "ON UPDATE CASCADE" "ouc" nil nil nil nil nil nil)
                       ("pk" "PRIMARY KEY ($0)," "pk" nil nil nil nil nil nil)
                       ("ref" "    REFERENCES ${1:table} (${$1_id})\n        ON UPDATE CASCADE\n        ON DELETE CASCADE\n" "references" nil nil nil nil nil nil)
                       ("ts" "${1:created_at} TIMESTAMP\n    NOT NULL\n    DEFAULT NOW()\n" "timestamp" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Dec  3 23:27:39 2014
