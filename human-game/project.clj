(defproject human-game "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.toomuchcode/clara-rules "0.11.0"]]
  :plugins [[lein-marginalia "0.9.0"]]
  :main ^:skip-aot human-game.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
