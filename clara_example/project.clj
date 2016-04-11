(defproject clara_example "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.toomuchcode/clara-rules "0.11.0"]]
  :main ^:skip-aot clara-example.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
