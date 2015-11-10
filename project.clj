(defproject collection-check "0.1.7-SNAPSHOT"
  :description "fuzz testing for alternate data structures"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/test.check "0.8.0"]
                 [com.gfredericks/test.chuck "0.1.21"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]]}})
