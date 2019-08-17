(defproject tracer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :profiles {:dev {:dependencies [[orchestra "2018.12.06-2"]
                                  [org.clojure/test.check "0.10.0-alpha4"]
                                  [criterium "0.4.5"]]}
             :fast {:jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :plugins [[lein-cloverage "1.1.1"]]
  :repl-options {:init-ns tracer.core}
  :global-vars {*warn-on-reflection* true})
