{:user {:plugins [[lein-kibit "0.0.8"]
                  [lein-midje "3.0.0"]
                  [lein-exec "0.3.1"]
                  [lein-ancient "0.5.4" :exclusions [org.clojure/clojure
                                                     org.clojure/tools.reader
                                                     commons-codec]]
                  [gluon "0.1.0-SNAPSHOT"]]
        :dependencies [[slamhound "1.5.1"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}



