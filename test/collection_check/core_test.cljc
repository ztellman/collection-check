(ns collection-check.core-test
  (:require
   #?(:clj [clojure.test :refer [deftest]]
      :cljs [cljs.test :refer-macros [deftest]])
   [collection-check.core :as cc]
   [clojure.test.check.generators :as gen]))

(def gen-element
  (gen/tuple gen/int))

(deftest test-identities
  (cc/assert-vector-like 100 [1] gen-element)
  (cc/assert-map-like 100 (sorted-map) gen-element gen-element {:base (sorted-map) :ordered? true})
  (cc/assert-map-like 100 {} gen-element gen-element)
  (cc/assert-set-like 100 (sorted-set) gen-element {:base (sorted-set) :ordered? true})
  (cc/assert-set-like 100 #{} gen-element))
