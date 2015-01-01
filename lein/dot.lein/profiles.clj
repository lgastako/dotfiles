{:user {:env {:env :dev ;; so you can check "(= (env :env) :dev)
              :febay-port 2014
              :febay-elastic-search-url "http://localhost:9200"}
        :plugins [[lein-kibit "0.0.8"]
                  [cider/cider-nrepl "0.8.2"]
                  [quickie "0.2.5"]
                  ;; [soup-site/lein-template "1.1.0-SNAPSHOT"]
                  ;; [lein-typed "0.3.4"]
                  ;; [lein-ritz "0.7.0"]
                  ;; [lein-cprint "1.0.0"]
                  ;; [lein-bin "0.3.4"]
                  ;; [lein-gorilla "0.2.0"]
                  ;; [lein-midje "3.0.0"]
                  [lein-exec "0.3.1"]
                  ;; [nikola "0.1.0-SNAPSHOT"]
                  ;; [xwatch "0.1.0-SNAPSHOT"]
                  [lein-simpleton "1.2.0"]
                  ;; [lein-pprint "1.1.1"]
                  ;; [lein-licenses "0.1.1"]
                  [lein-ancient "0.5.4" :exclusions [org.clojure/clojure
                                                     org.clojure/tools.reader
                                                     commons-codec]]
                  [jonase/eastwood "0.1.0"]
                  ;; [hiccup-bridge "1.0.0-SNAPSHOT"]
                  ;; [gluon "0.1.0-SNAPSHOT"]
                  ;; [com.jakemccrary/lein-test-refresh "0.3.4"]
                  ;; [slamhound "1.5.1"]
                  ;; [lein-vanity "0.2.0"]
                  [lein-try "0.4.1"]]
        :dependencies [
                       ;; [slamhound "1.5.1"]
                       ;; [ritz/ritz-nrepl-middleware "0.7.0"]
                       ;; [nrepl-inspect "0.3.0"]
;;                       [schmetterling "0.0.7"]
                       [alembic "0.2.1"]
;;                       [io.aviso/pretty "0.1.8"]
                       ]
        ;; :aliases {"slamhound" ["run" "-m" "slam.hound"]
        ;;           "auto-test" ["do" "clean," "cljsbuild" "auto" "test"]}
        ;; :injections  [(require 'io.aviso.repl
        ;;                        'clojure.repl
        ;;                        'clojure.main)
        ;;               (alter-var-root #'clojure.main/repl-caught
        ;;                               (constantly @#'io.aviso.repl/pretty-pst))
        ;;               (alter-var-root #'clojure.repl/pst
        ;;                               (constantly @#'io.aviso.repl/pretty-pst))]
        ;; :repl-options {:nrepl-middleware [ritz.nrepl.middleware.javadoc/wrap-javadoc
        ;;                                   ritz.nrepl.middleware.simple-complete/wrap-simple-complete
        ;;                                   ritz.nrepl.middleware.apropos/wrap-apropos
        ;;                                   inspector.middleware/wrap-inspect]}
        }}
