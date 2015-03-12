{:user {:env {:env :dev ;; so you can check "(= (env :env) :dev)
              }
        :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [lein-ancient "0.6.5" :exclusions [org.clojure/clojure
                                                     org.clojure/tools.reader
                                                     commons-codec]]
                  #_ [lein-bin "0.3.4"]
                  #_ [lein-exec "0.3.1"]
                  #_ [lein-kibit "0.0.8"]
                  #_ [lein-simpleton "1.2.0"]
                  #_ [lein-try "0.4.1"]
                  #_ [jonase/eastwood "0.1.0"]
                  ]}}
