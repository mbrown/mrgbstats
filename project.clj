(defproject mrgbstats "0.1.1-SNAPSHOT"
  :description "Basic statistics library"
  :url "http://github.com/mbrown/mrgbstats/"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [random-seed "1.0.0"]
                 ;[net.mikera/core.matrix "0.62.0"]
                 ]
  :repl-options {:init-ns mrgbstats.core})
