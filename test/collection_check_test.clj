(ns collection-check-test
  (:require
    [clojure.test :refer :all]
    [collection-check :refer :all]
    [clojure.test.check.generators :as gen]))

(def gen-element
  (gen/tuple gen/int))

(deftest test-identities
  (assert-vector-like 100 [] gen-element)
  (assert-map-like 100 (sorted-map) gen-element gen-element {:base (sorted-map) :ordered? true})
  (assert-map-like 100 {} gen-element gen-element)
  (assert-set-like 100 (sorted-set) gen-element {:base (sorted-set) :ordered? true})
  (assert-set-like 100 #{} gen-element))
