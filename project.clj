(defproject looping-is-recursion "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [iloveponies.tests/looping-is-recursion "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[midje "1.9.5" :exclusions [org.clojure/clojure]]]
                   :plugins      [[lein-midje "3.2.1"]]}})

