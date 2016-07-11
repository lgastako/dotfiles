{:user {;;:dependencies [[org.clojure/tools.namespace "0.2.3"]]
        :dependencies [[org.clojure/tools.namespace "0.2.11"]]
        :injections [(require '[clojure.tools.namespace.repl :refer [refresh]])
                     ;; (require '[clojure.tools.namespace.find :as find])
                     (require '[clojure.pprint :refer :all])
                     (require '[clojure.repl :refer :all])]
        :plugins [;; [cider/cider-nrepl "0.12.0"]
                  [lein-ancient      "0.6.10"]
                  [lein-auto         "0.1.2"]
                  [lein-bikeshed     "0.3.0"]
                  [jonase/eastwood   "0.2.3"]
                  [lein-pprint       "1.1.1"]
                  [lein-kibit        "0.1.2"]]}}
