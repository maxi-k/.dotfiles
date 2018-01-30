{:user
 {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                 [spyscope "0.1.5"]
                 [criterium "0.4.3"]]
  :injections [(require '(clojure.tools.namespace repl find))
               ;; try/catch to work around an issue where 'lein repl'
               ;; outside a project dir will not load reader literal
               ;; definitions correctly
               (try (require 'spyscope.core)
                    (catch RuntimeException e))]
  :plugins [[lein-exec "0.3.6"]
            [lein-auto "0.1.3"]
            [lein-cooper "1.2.2"]
            [cider/cider-nrepl "0.15.1"]
            [refactor-nrepl "2.3.1"]]
  }
 :cuttle {:plugins [[lein-pprint "1.1.1"]]}}
