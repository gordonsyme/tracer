(defproject tracer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:dependencies [[orchestra "2019.02.06-1"]
                                  [org.clojure/test.check "1.0.0"]
                                  [criterium "0.4.5"]]}
             :fast {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]
                    :global-vars {*unchecked-math* true}}}
  :plugins [[lein-cloverage "1.1.1"]]
  :aliases {"coverage" ["cloverage" "--ns-exclude-regex" "putting-together\\..*"]}
  :repl-options {:init-ns tracer.core}
  :global-vars {*warn-on-reflection* true})
