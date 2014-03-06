{:user {:plugins [[lein-kibit "0.0.8"]
                  [lein-ritz "0.7.0"]
                  [lein-midje "3.0.0"]
                  [lein-exec "0.3.1"]
                  ;; [xwatch "0.1.0-SNAPSHOT"]
                  [lein-simpleton "1.2.0"]
                  [lein-pprint "1.1.1"]
                  [lein-ancient "0.5.4" :exclusions [org.clojure/clojure
                                                     org.clojure/tools.reader
                                                     commons-codec]]
                  [jonase/eastwood "0.1.0"]
                  [hiccup-bridge "1.0.0-SNAPSHOT"]
                  ;; [gluon "0.1.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.3.4"]
                  [slamhound "1.5.1"]]
        :dependencies [[slamhound "1.5.1"]
                       [ritz/ritz-nrepl-middleware "0.7.0"]
                       [nrepl-inspect "0.3.0"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :repl-options {:nrepl-middleware [ritz.nrepl.middleware.javadoc/wrap-javadoc
                                          ritz.nrepl.middleware.simple-complete/wrap-simple-complete
                                          ritz.nrepl.middleware.apropos/wrap-apropos
                                          inspector.middleware/wrap-inspect]}}}
