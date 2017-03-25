(defproject clojure-stats "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [criterium "0.4.4"]
                 [net.mikera/core.matrix "0.51.0"]]
  :repl-options {:init (set! *print-length* 100)}
  ;:source-paths ["src/clojure" "src/clojure/core" "src/clojure/stats"]
  :java-source-paths ["src/stats/java"])
