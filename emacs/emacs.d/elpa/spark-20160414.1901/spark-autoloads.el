;;; spark-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "spark" "spark.el" (22708 48852 0 0))
;;; Generated autoloads from spark.el

(autoload 'spark "spark" "\
Generates a sparkline string for a list of real numbers.

Usage: SPARK <numbers> &key <min> <max> <key>

  * <numbers> ::= <list> of <real-number>
  * <min>     ::= { <null> | <real-number> }, default is NIL
  * <max>     ::= { <null> | <real-number> }, default is NIL
  * <key>     ::= <function>

  * <numbers> ~ data.
  * <min>    ~ lower bound of output.
               NIL means the minimum value of the data.
  * <max>    ~ upper bound of output.
               NIL means the maximum value of the data.
  * <key>    ~ function for preparing data.

Examples:

  (spark '(1 0 1 0))     => \"█▁█▁\"
  (spark '(1 0 1 0 0.5)) => \"█▁█▁▄\"
  (spark '(1 0 1 0 -1))  => \"█▄█▄▁\"

  (spark '(0 30 55 80 33 150))                 => \"▁▂▃▅▂█\"
  (spark '(0 30 55 80 33 150) :min -100)       => \"▃▄▅▆▄█\"
  (spark '(0 30 55 80 33 150) :max 50)         => \"▁▅██▅█\"
  (spark '(0 30 55 80 33 150) :min 30 :max 80) => \"▁▁▄█▁█\"

  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi (/ 1.0 4)))))
  => \"▄▆█▆▄▂▁▂▄\"
  (spark '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (cos (* x pi (/ 1.0 4)))))
  => \"█▆▄▂▁▂▄▆█\"

\(fn NUMBERS &key MIN MAX KEY)" nil nil)

(autoload 'spark-v "spark" "\
Generates a vertical sparkline string for a list of real numbers.

Usage: SPARK-V <numbers> &key <min> <max> <key> <size>
                             <labels> <title> <scale?> <newline?>

  * <numbers>  ::= <list> of <real-number>
  * <min>      ::= { <null> | <real-number> }, default is NIL
  * <max>      ::= { <null> | <real-number> }, default is NIL
  * <key>      ::= <function>
  * <size>     ::= <integer 1 *>, default is 50
  * <labels>   ::= <list>
  * <title>    ::= <object>, default is NIL
  * <scale?>   ::= <generalized-boolean>, default is T
  * <newline?> ::= <generalized-boolean>, default is T

  * <numbers>  ~ data.
  * <min>      ~ lower bound of output.
                 NIL means the minimum value of the data.
  * <max>      ~ upper bound of output.
                 NIL means the maximum value of the data.
  * <key>      ~ function for preparing data.
  * <size>     ~ maximum number of output columns (contains label).
  * <labels>   ~ labels for data.
  * <title>    ~ If title is too big for size, it is not printed.
  * <scale?>   ~ If T, output graph with scale for easy to see.
                 If string length of min and max is too big for size,
                 the scale is not printed.
  * <newline?> ~ If T, output graph with newlines for easy to see.


Examples:

  ;; Life expectancy by WHO region, 2011, bothsexes
  ;; see. http://apps.who.int/gho/data/view.main.690
  (defvar life-expectancies '((\"Africa\" 56)
                              (\"Americans\" 76)
                              (\"South-East Asia\" 67)
                              (\"Europe\" 76)
                              (\"Eastern Mediterranean\" 68)
                              (\"Western Pacific\" 76)
                              (\"Global\" 70)))

  (spark-v life-expectancies :key #'second :scale? nil :newline? nil)
  =>
  \"▏
  ██████████████████████████████████████████████████
  ███████████████████████████▌
  ██████████████████████████████████████████████████
  ██████████████████████████████▏
  ██████████████████████████████████████████████████
  ███████████████████████████████████▏\"

  (spark-v life-expectancies :min 50 :max 80
                             :key    #'second
                             :labels (mapcar #'first life-expectancies)
                             :title \"Life Expectancy\")
  =>
  \"
                   Life Expectancy
                        50           65           80
                        ˫------------+-------------˧
                 Africa █████▋
              Americans ████████████████████████▎
        South-East Asia ███████████████▉
                 Europe ████████████████████████▎
  Eastern Mediterranean ████████████████▊
        Western Pacific ████████████████████████▎
                 Global ██████████████████▋
  \"

  (spark-v '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi (/ 1.0 4))))
                                :size 20)
  \"
  -1.0     0.0     1.0
  ˫---------+--------˧
  ██████████▏
  █████████████████▏
  ████████████████████
  █████████████████▏
  ██████████▏
  ██▉
  ▏
  ██▉
  █████████▉
  \"

  (spark-v '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi (/ 1.0 4))))
                                :size 10)
  =>
  \"
  -1.0   1.0
  ˫--------˧
  █████▏
  ████████▏
  ██████████
  ████████▏
  █████▏
  █▏
  ▏
  █▏
  ████▏
  \"

  (spark-v '(0 1 2 3 4 5 6 7 8) :key (lambda (x) (sin (* x pi (/ 1.0 4))))
                                :size 1)
  =>
  \"
  ▌
  ▊
  █
  ▊
  ▌
  ▎
  ▏
  ▎
  ▌
  \"

\(fn NUMBERS &key MIN MAX KEY (size 50) LABELS TITLE (scale\\? t) (newline\\? t))" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; spark-autoloads.el ends here
