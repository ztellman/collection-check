(set-env! :source-paths #{"src"}
          :dependencies '[[adzerk/bootlaces "0.1.13" :scope "test"]
                          [adzerk/boot-test "1.0.5" :scope "test"]
                          [crisptrutski/boot-cljs-test "0.2.0-SNAPSHOT" :scope "test"]
                          [org.clojure/test.check "0.8.0"]
                          [com.gfredericks/test.chuck "0.2.1"]])

(require '[adzerk.boot-test :refer [test]]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]]
         '[adzerk.bootlaces :as b :refer [build-jar push-release push-snapshot]])

(def +version+ "0.1.7-SNAPSHOT")
(b/bootlaces! +version+)

(task-options! pom {:project 'collection-check
                    :version +version+
                    :description "fuzz testing for alternate data structures"
                    :url "https://github.com/ztellman/collection-check"
                    :license {"MIT" "http://opensource.org/licenses/MIT"}})

(deftask testing []
  (merge-env! :source-paths #{"test"})
  (let [nss #{'collection-check.core-test}]
    (task-options! test-cljs {:namespaces nss}
                   test      {:namespaces nss}))
  identity)

(deftask test-cljc []
  (comp (test-cljs) (test)))

;; run tests with `boot testing test-cljc`
;; just clojure tests with `boot testing test`
;; just cljs tests with `boot testing test-cljs`
