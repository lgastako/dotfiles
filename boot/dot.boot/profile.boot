(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.8.2"]])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)

(set-env! :dependencies '[[boot-deps "0.1.6"]])
(require '[boot-deps :refer [ancient]])
