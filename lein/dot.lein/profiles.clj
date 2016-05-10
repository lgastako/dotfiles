{:repl {:plugins [
                  ;;[cider/cider-nrepl "0.9.1"]
                  ;; [cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  ;; [cider/cider-nrepl "0.10.0"]
                  ;; [cider/cider-nrepl "0.11.0-SNAPSHOT"]
                  [cider/cider-nrepl "0.11.0"]

                  [org.clojure/tools.nrepl "0.2.12"
                   :exclusions [org.clojure/clojure]]
                  ;; [org.clojure/tools.nrepl "0.2.10"
                  ;;  :exclusions [org.clojure/clojure]]

                  ;; [refactor-nrepl "1.1.0"]
                  [refactor-nrepl "2.2.0"]
                  ]}
 :user {:plugins [[org.clojure/clojurescript "1.7.170"] ;; needed to prevent breakage in ASP with vinyasa.lein
                  [jonase/eastwood         "0.2.1"]
                  [lein-ancient            "0.6.5"
                   :exclusions [org.clojure/clojure
                                org.clojure/core.cache
                                org.clojure/tools.reader
                                commons-codec]]
                  [ancient-clj              "0.3.6"
                   :exclusions [com.amazonaws/aws-java-sdk-s3]]
                  [clj-http                 "1.0.1"
                   :exclusions [joda-time org.clojure/clojure
                                com.cognitect/transit-clj]]
                  [slingshot                "0.10.3"
                   :exclusions [org.clojure/clojure]]
                  [lein-auto                "0.1.2"]
                  [lein-bikeshed            "0.2.0"]
                  [lein-cloverage           "1.0.2"]
                  [lein-codox               "0.9.0"]
                  ;; [lein-instant-cheatsheet "2.1.4"
                  ;;  :exclusions [org.clojure/tools.namespace]]
                  [lein-create-template     "0.1.2"]
                  [lein-fruit               "0.2.3"]
                  [lein-hiera               "0.9.5"]
                  [lein-kibit               "0.1.2"]
                  [lein-localrepo           "0.5.3"]
                  [lein-marginalia          "0.8.0"]
                  [lein-ns-dep-graph        "0.1.0-SNAPSHOT"]
                  [lein-vanity              "0.2.0"]
                  ;; [org.timmc/nephila       "0.3.0"]
                  [lein-pprint              "1.1.1"]
                  ;; [refactor-nrepl           "1.1.0"]
                  [refactor-nrepl           "2.2.0"]
                  ;; [varspotting             "0.0.2"]
                  ;; [venantius/yagni         "0.1.1"]
                  ]
        :ios {:robovm-path "/Users/john/Downloads/robovm-1.4.0"}
        :dependencies [[alembic "0.3.2"]
                       ;; Had to add this manually for some reason to avoid
                       ;; problems starting a REPL.
                       [commons-logging/commons-logging "1.2"]
                       [com.cemerick/pomegranate        "0.3.1"]
                       [com.gfredericks/debug-repl      "0.0.7"]
                       ;; [com.inferstructure/repl         "0.1.0-SNAPSHOT"]
                       [im.chit/vinyasa                 "0.4.2"]
                       ;; [io.aviso/pretty                 "0.1.8"]
                       [leiningen #=(leiningen.core.main/leiningen-version)
                        :exclusions [commons-logging
                                     org.apache.httpcomponents/httpclient
                                     org.apache.maven.wagon/wagon-provider-api
                                     org.codehaus.plexus/plexus-utils]]
                       [org.clojure/tools.namespace     "0.2.4"]
                       [spyscope "0.1.5"]]
        :repl-options {:nrepl-middleware [com.gfredericks.debug-repl/wrap-debug-repl]}
        :injections [(require 'leiningen.core.main)
                     (require 'spyscope.core)
                     (require '[vinyasa.inject :as inject])
                     ;; (require 'io.aviso.repl)
                     ;; (require '[com.inferstructure.repl])

                     ;; ;; the default injected namespace is `.`
                     ;; ;; note that `:refer, :all and :exclude can be used
                     (inject/in [vinyasa.inject :refer [inject [in inject-in]]]
                                [vinyasa.lein :exclude [*project*]]

                                ;; imports all functions in vinyasa.pull
                                [alembic.still [distill pull]]

                                ;; inject into clojure.core
                                clojure.core
                                [vinyasa.reflection .> .? .* .% .%> .& .>ns .>var]

                                ;; inject into clojure.core with prefix
                                clojure.core >
                                ;; [com.gfredericks.debug-repl break! catch-break! unbreak! unbreak!!]
                                [clojure.java.shell sh]
                                [clojure.pprint pprint]
                                ;; [com.inferstructure.repl explore]
                                )]}}
