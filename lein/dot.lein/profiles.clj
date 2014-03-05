{:user {:plugins [[lein-kibit "0.0.8"]
                  [lein-midje "3.0.0"]
                  [lein-exec "0.3.1"]
                  [lein-ancient "0.5.4" :exclusions [org.clojure/clojure
                                                     org.clojure/tools.reader
                                                     commons-codec]]
                  [jonase/eastwood "0.1.0"]
;;                  [gluon "0.1.0-SNAPSHOT"]
;;                  [com.jakemccrary/lein-test-refresh "0.3.4"]
                  [slamhound "1.5.1"]
                  [lein-vanity "0.2.0"]
                  [lein-try "0.4.1"]]
        :dependencies [[slamhound "1.5.1"]
                       [schmetterling "0.0.7"]
                       [alembic "0.2.1"]
                       [io.aviso/pretty "0.1.8"]]
        :injections  [(require 'io.aviso.repl
                               'clojure.repl
                               'clojure.main)
                      (alter-var-root #'clojure.main/repl-caught
                                      (constantly @#'io.aviso.repl/pretty-pst))
                      (alter-var-root #'clojure.repl/pst
                                      (constantly @#'io.aviso.repl/pretty-pst))]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}



