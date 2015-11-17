;;; Compiled snippets and support files for `html-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
                     '(("h" "<!DOCTYPE html>\n<html lang=\"en\">\n  <head>\n    <meta charset=\"utf-8\">\n    <title>${1:Index}</title>\n  <body>\n    <script src=\"js/${2:js/index}.js\"></script>\n$0\n" "html" nil nil nil nil nil nil)
                       ("l" "<link href=\"$0\" rel=\"stylesheet\">\n" "link" nil nil nil nil nil nil)
                       ("s" "<script src=\"$0\"></script>" "script" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Wed Dec  3 23:27:39 2014
