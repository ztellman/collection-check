(ns collection-check.core
  (:require
    [clojure.string :as str]
    [clojure.test.check :refer (quick-check)]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    ;; Macros Clojure
    #?(:clj [clojure.test :refer [is]])
    #?(:clj [collection-check.macros :as m])
    #?(:clj [com.gfredericks.test.chuck.clojure-test :refer [checking]])
    ;; Make sure CLJS macro namespaces are required
    #?(:cljs [com.gfredericks.test.chuck.clojure-test]))
  ;; Macros ClojureScript
  #?(:cljs (:require-macros [com.gfredericks.test.chuck.clojure-test :refer [checking]]
                            [collection-check.macros :as m]
                            [cljs.test :refer [is]]))
  #?(:clj (:import [java.util Collection List Map])))

#?(:clj (set! *warn-on-reflection* false))

;;;

(defn gen-meta [gen]
  (gen/fmap
    (fn [[x meta]]
      (if #?(:clj (instance? clojure.lang.IObj x)
             :cljs (satisfies? cljs.core/IWithMeta x)) ;; CLJC
        (with-meta x {:foo meta})
        x))
    (gen/tuple
      gen
      (gen/one-of [gen/int (gen/return nil)]))))

(defn- tuple* [& args]
  (->> args
    (map
      #(if (and (map? %) (contains? % :gen))
         %
         (gen/return %)))
    (apply gen/tuple)))

(defn- ttuple [& args]
  (gen/tuple (apply tuple* args)))

;;;

(defn- seq-actions [element-generator]
  (gen/fmap
    (fn [[pre actions post]]
      (concat [pre] actions [post]))
    (tuple*
      [:seq]
      (gen/list
        (gen/one-of
          [(tuple* :rest)
           (tuple* :map-id)
           (tuple* :cons element-generator)
           (tuple* :conj element-generator)]))
      [:into])))

(defn- transient-actions [& intermediate-actions]
  (gen/fmap
    (fn [[pre actions post]]
      (concat [pre] actions [post]))
    (tuple*
      [:transient]
      (gen/list
        (gen/one-of
          (vec intermediate-actions)))
      [:persistent!])))

(defn- gen-vector-actions [element-generator transient? ordered?]
  (let [element-generator (gen-meta element-generator)
        standard [(ttuple :pop)
                  (ttuple :conj element-generator)
                  (ttuple :assoc (gen/choose 0 1e3) element-generator)]]
    (gen/list
      (gen/one-of
        (concat
          standard
          (when transient?
            [(transient-actions
               (tuple* :conj! element-generator)
               (tuple* :assoc! (gen/choose 0 999) element-generator)
               (tuple* :pop!))])
          (when ordered?
            [(seq-actions element-generator)]))))))

(defn- gen-map-actions [key-generator value-generator transient? ordered?]
  (let [key-generator (gen-meta key-generator)
        value-generator (gen-meta value-generator)
        standard [(ttuple :dissoc key-generator)
                  (ttuple :assoc key-generator value-generator)]]
    (gen/list
      (gen/one-of
        (concat
          standard
          (when transient?
            [(transient-actions
               (tuple* :dissoc! key-generator)
               (tuple* :assoc! key-generator value-generator))])
          (when ordered?
            [(seq-actions (tuple* key-generator value-generator))]))))))

(defn- gen-set-actions [element-generator transient? ordered?]
  (let [element-generator (gen-meta element-generator)
        standard [(ttuple :conj element-generator)
                  (ttuple :disj element-generator)]]
    (gen/list
      (gen/one-of
        (concat
          standard
          (when transient?
            [(transient-actions
               (tuple* :conj! element-generator)
               (tuple* :disj! element-generator))])
          (when ordered?
            [(seq-actions element-generator)]))))))

(defn- transient? [x]
  (instance? clojure.lang.IEditableCollection x)) ;; CLJC

(defn- build-collections
  "Given a list of actions, constructs two parallel collections that can be compared
   for equivalencies."
  [coll-a coll-b vector-like? actions]
  (let [orig-a coll-a
        orig-b coll-b]
    (reduce
      (fn [[coll-a coll-b prev-actions] [action x y :as act]]

        ;; if it's a vector, we need to choose a valid index
        (let [idx (when (and vector-like? (#{:assoc :assoc!} action))
                    (if (< 900 x)
                      (count coll-a)
                      (int (* (count coll-a) (/ x 1e3)))))
              [action x y :as act] (if idx
                                     [action idx y]
                                     act)
              f (case action
                  :cons #(cons x %)
                  :rest rest
                  :map-id #(map identity %)
                  :seq seq
                  :into [#(into (empty orig-a) %) #(into (empty orig-b) %)]
                  :persistent! persistent!
                  :transient transient
                  :pop #(if (empty? %) % (pop %))
                  :pop! #(if (= 0 (count %)) % (pop! %))
                  :conj #(conj % x)
                  :conj! #(conj! % x)
                  :disj #(disj % x)
                  :disj! #(disj! % x)
                  :assoc #(assoc % x y)
                  :assoc! #(assoc! % x y)
                  :dissoc #(dissoc % x)
                  :dissoc! #(dissoc! % x))
              f-a (if (vector? f) (first f) f)
              f-b (if (vector? f) (second f) f)]

          [(f-a coll-a)
           (f-b coll-b)
           (conj prev-actions act)]))
      [coll-a coll-b []]
      (apply concat actions))))

;;;

;; indirection for 1.4 compatibility
;; (def reduced* (ns-resolve 'clojure.core 'reduced)) ;; CLJC

(defn assert-equivalent-collections
  [a b]
  (is (= (count a) (count b) (.size a) (.size b)))
  (is (= a b))
  (is (= b a))
  (is (.equals ^Object a b))
  (is (.equals ^Object b a))
  (is (= (hash a) (hash b)))
  (is (= (.hashCode ^Object a) (.hashCode ^Object b)))
  (is (= a b
            (into (empty a) a)
            (into (empty b) b)
            (into (empty a) b)
            (into (empty b) a)))
  (is (= (into (empty a) (take 1 a))
         (reduce #(reduced (conj %1 %2)) (empty a) a)))
  (is (= (into (empty b) (take 1 b))
         (reduce #(reduced (conj %1 %2)) (empty b) b))))

(defn- meta-map [s]
  (->> s (map #(vector % (meta %))) (into {})))

(defn assert-equivalent-vectors [a b]
  (assert-equivalent-collections a b)
  (is (= (first a) (first b)))
  (is (= (map #(nth a %) (range (count a)))
            (map #(nth b %) (range (count b)))))
  (is (= (map #(a %) (range (count a)))
            (map #(b %) (range (count b)))))
  (is (= (map #(get a %) (range (count a)))
            (map #(get b %) (range (count b)))))
  (is (= (map #(.get ^List a %) (range (count a)))
            (map #(.get ^List b %) (range (count b)))))
  (is (= 0 (compare a b))))

(defn assert-equivalent-sets [a b]
  (assert-equivalent-collections a b)
  (is (= (set (map #(a %) a))
            (set (map #(b %) b))))
  (is (= (meta-map a) (meta-map b)))
  (is (and
            (every? #(contains? a %) b)
            (every? #(contains? b %) a))))

(defn assert-equivalent-maps [a b]
  (assert-equivalent-collections a b)
  (is (= (set (keys a)) (set (keys b))))
  (let [ks (keys a)]
    (is (= (map #(get a %) ks)
           (map #(get b %) ks)))
    (is (= (map #(a %) ks)
           (map #(b %) ks)))
    (is (= (map #(.get ^Map a %) ks)
           (map #(.get ^Map b %) ks))))
  (is (and (every? #(= (key %) (first %)) a)
           (every? #(= (key %) (first %)) b)))
  (is (= (meta-map (keys a)) (meta-map (keys b))))
  (is (every? #(= (meta (a %)) (meta (b %))) (keys a)))
  (is (every? #(= (val %) (a (key %)) (b (key %))) a)))

;;;

(defn pr-meta [v]
  (if-let [m (meta v)]
    `(with-meta ~v ~m)
    v))

(defn describe-action [[f & rst]]
  (case f
    :cons (list '->> (list* 'cons (map pr-meta rst)))
    :into '(into (empty coll))
    (if (empty? rst)
      (symbol (name f))
      (list*
        (symbol (name f))
        (map pr-meta rst)))))

(defn report-failing-actions [x]
  (when (and (= :fail (:type x))
             (get-in x [:message :shrunk]))
    (let [actions (get-in x [:message :shrunk :smallest])]
      (println "\n  actions = " (->> actions
                                     first
                                     (apply concat)
                                     (map describe-action)
                                     (list* '-> 'coll)
                                     pr-str)
               "\n"))))

;;

(defn assert-vector-like
  "Asserts that the given empty collection behaves like a vector."
  ([empty-coll element-generator]
     (assert-vector-like 1e3 empty-coll element-generator nil))
  ([n empty-coll element-generator]
     (assert-vector-like n empty-coll element-generator nil))
  ([n empty-coll element-generator
    {:keys [base ordered?]
     :or {ordered? true
          base []}}]
       (#?(:clj  m/clj-reporting-failing-actions
           :cljs m/cljs-reporting-failing-actions)
         (checking "vector-like" n
           [actions (gen-vector-actions element-generator (transient? empty-coll) ordered?)]
           (let [[a b actions] (build-collections empty-coll base true actions)]
             (assert-equivalent-vectors a b))))))

(defn assert-set-like
  ([empty-coll element-generator]
     (assert-set-like 1e3 empty-coll element-generator nil))
  ([n empty-coll element-generator]
     (assert-set-like n empty-coll element-generator nil))
  ([n empty-coll element-generator
    {:keys [base ordered?]
     :or {ordered? false
          base #{}}}]
     (#?(:clj  m/clj-reporting-failing-actions
         :cljs m/cljs-reporting-failing-actions)
       (checking "set-like" n
         [actions (gen-set-actions element-generator (transient? empty-coll) ordered?)]
         (let [[a b actions] (build-collections empty-coll base false actions)]
           (assert-equivalent-sets a b))))))

(defn assert-map-like
  ([empty-coll key-generator value-generator]
     (assert-map-like 1e3 empty-coll key-generator value-generator nil))
  ([n empty-coll key-generator value-generator]
     (assert-map-like n empty-coll key-generator value-generator nil))
  ([n empty-coll key-generator value-generator
    {:keys [base ordered?]
     :or {ordered? false
          base {}}}]
   (#?(:clj  m/clj-reporting-failing-actions
       :cljs m/cljs-reporting-failing-actions)
       (checking "map-like" n
         [actions (gen-map-actions key-generator value-generator (transient? empty-coll) ordered?)]
         (let [[a b actions] (build-collections empty-coll base false actions)]
           (assert-equivalent-maps a b))))))
