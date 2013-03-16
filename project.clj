(defproject cellflow "0.1.1"
  :description "Cellflow: Cashflow solving via dataflow DSL"
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.apache.commons/commons-math "2.2"]
                 [clj-time "0.4.5"]
                 [org.clojure/data.csv "0.1.2"]]
  :profiles {:dev
             {:dependencies [[midje "1.5.0"]]
              :plugins [[lein-midje "3.0.0"]]}})
