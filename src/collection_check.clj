(ns collection-check
  (:require
    [clojure.string :as str]
    [simple-check.generators :as gen]
    [simple-check.properties :as prop]
    [simple-check.core :refer (quick-check)]))

;;;

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

(defn- gen-vector-actions [element-generator transient?]
  (let [standard [(ttuple :pop)
                  (ttuple :conj element-generator)
                  (ttuple :assoc (gen/choose 0 1e3) element-generator)]
        transient (gen/fmap
                    (fn [[pre actions post]]
                      (concat [pre] actions [post]))
                    (tuple*
                      [:transient]
                      (gen/list
                        (gen/one-of
                          [(tuple* :conj! element-generator)
                           (tuple* :assoc! (gen/choose 0 999) element-generator)
                           (tuple* :pop!)]))
                      [:persistent!]))]
    (gen/one-of
      (if transient?
        (conj standard transient)
        standard))))

(defn- gen-map-actions [key-generator value-generator transient?]
  (let [standard [(ttuple :dissoc key-generator)
                  (ttuple :assoc key-generator value-generator)]
        transient (gen/fmap
                    (fn [[pre actions post]]
                      (concat [pre] actions [post]))
                    (tuple*
                      [:transient]
                      (gen/list
                        (gen/one-of
                          [(tuple* :dissoc! key-generator)
                           (tuple* :assoc! key-generator value-generator)]))
                      [:persistent!]))]
    (gen/one-of
      (if transient?
        (conj standard transient)
        standard))))

(defn- gen-set-actions [element-generator transient?]
  (let [standard [(ttuple :conj element-generator)
                  (ttuple :disj element-generator)]
        transient (gen/fmap
                    (fn [[pre actions post]]
                      (concat [pre] actions [post]))
                    (tuple*
                      [:transient]
                      (gen/list
                        (gen/one-of
                          [(tuple* :conj! element-generator)
                           (tuple* :disj! element-generator)]))
                      [:persistent!]))]
    (gen/one-of
      (if transient?
        (conj standard transient)
        standard))))

(defn- transient? [x]
  (instance? clojure.lang.IEditableCollection x))

(defn- gen-collections
  "Given an action generator, constructs two parallel collections that can be compared
   for equivalencies."
  [coll-a coll-b vector-like? action-generator]
  (let [transient? (and (transient? coll-a) (transient? coll-b))]
    (gen/fmap
      (fn [action-lists]
        (reduce
          (fn [coll+coll+actions actions]
            (reduce
              (fn [[coll-a coll-b prev-actions] [action x y :as act]]

                ;; if it's a vector, we need to choose a valid index
                (let [idx (when (and vector-like? (#{:assoc :assoc!} action))
                            (if (< 900 x)
                              (count coll-a)
                              (int (* (count coll-a) (/ x 1e3)))))
                      f (case action
                          :persistent! persistent!
                          :transient transient
                          :pop #(if (empty? %) % (pop %))
                          :pop! #(if (= 0 (count %)) % (pop! %))
                          :conj #(conj % x)
                          :conj! #(conj! % x)
                          :disj #(disj % x)
                          :disj! #(disj! % x)
                          :assoc #(if idx
                                    (assoc % idx y)
                                    (assoc % x y))
                          :assoc! #(if idx
                                     (assoc! % idx y)
                                     (assoc! % x y))
                          :dissoc #(dissoc % x)
                          :dissoc! #(dissoc! % x))]
                  [(f coll-a)
                   (f coll-b)
                   (conj
                     prev-actions
                     (if idx
                       [action idx y]
                       act))]))
              coll+coll+actions
              actions))
          [coll-a coll-b []]
          action-lists))
      (gen/list action-generator))))

(defn gen-vector-like
  "Returns a tuple of the given collection, a reference vector, and the actions taken to
   construct it."
  [empty-coll element-generator]
  (gen-collections
    empty-coll
    []
    true
    (gen-vector-actions element-generator (transient? empty-coll))))

(defn gen-set-like
  "Returns a tuple of the given collection, a reference set, and the actions taken to
   construct it."
  [empty-coll element-generator]
  (gen-collections
    empty-coll
    #{}
    false
    (gen-set-actions element-generator (transient? empty-coll))))

(defn gen-map-like
  "Returns a tuple of the given collection, a reference map, and the actions taken to
   construct it."
  [empty-coll key-generator value-generator]
  (gen-collections
    empty-coll
    {}
    false
    (gen-map-actions key-generator value-generator (transient? empty-coll))))

;;;

(defn assert-equivalent-collections
  [a b]
  (assert (= (count a) (count b) (.size a) (.size b)))
  (assert (= a b))
  (assert (= b a))
  (assert (.equals a b))
  (assert (.equals b a))
  (assert (= (hash a) (hash b)))
  (assert (= (.hashCode a) (.hashCode b)))
  (assert (= a b
            (into (empty a) a)
            (into (empty b) b)
            (into (empty a) b)
            (into (empty b) a)))
  (assert (= (conj (empty a) (first a))
            (reduce #(reduced (conj %1 %2)) (empty a) [(first a)])))
  (assert (= (conj (empty b) (first b))
            (reduce #(reduced (conj %1 %2)) (empty b) [(first b)]))))

(defn assert-equivalent-vectors [a b]
  (assert-equivalent-collections a b)
  (assert (= (first a) (first b)))
  (assert (= (map #(nth a %) (range (count a)))
            (map #(nth b %) (range (count b)))))
  (assert (= (map #(a %) (range (count a)))
            (map #(b %) (range (count b)))))
  (assert (= (map #(get a %) (range (count a)))
            (map #(get b %) (range (count b)))))
  (assert (= (map #(.get a %) (range (count a)))
            (map #(.get b %) (range (count b)))))
  (assert (= 0 (compare a b))))

(defn assert-equivalent-sets [a b]
  (assert-equivalent-collections a b)
  (assert (= (set (map #(a %) a))
            (set (map #(b %) b))))
  (assert (and
            (every? #(contains? a %) b)
            (every? #(contains? b %) a))))

(defn assert-equivalent-maps [a b]
  (assert-equivalent-collections a b)
  (assert (= (set (keys a)) (set (keys b))))
  (let [ks (keys a)]
    (assert (= (map #(get a %) ks)
              (map #(get b %) ks)))
    (assert (= (map #(a %) ks)
              (map #(b %) ks)))
    (assert (= (map #(.get a %) ks)
              (map #(.get b %) ks))))
  (assert (and
            (every? #(= (key %) (first %)) a)
            (every? #(= (key %) (first %)) b)))
  (assert (every? #(= (val %) (a (key %)) (b (key %))) a)))

;;;

(defn- assert-not-failed [x]
  (if (:fail x)
    (throw (Exception. (pr-str x)))
    x))

(defn assert-vector-like
  "Asserts that the given empty collection behaves like a vector."
  ([empty-coll element-generator]
     (assert-vector-like 1e3 empty-coll element-generator))
  ([n empty-coll element-generator]
     (assert-not-failed
       (quick-check n
         (prop/for-all [[a b actions] (gen-vector-like empty-coll element-generator)]
           (assert-equivalent-vectors a b)
           true)))))

(defn assert-set-like
  ([empty-coll element-generator]
     (assert-set-like 1e3 empty-coll element-generator))
  ([n empty-coll element-generator]
     (assert-not-failed
       (quick-check n
         (prop/for-all [[a b actions] (gen-set-like empty-coll element-generator)]
           (assert-equivalent-sets a b)
           true)))))

(defn assert-map-like
  ([empty-coll element-generator]
     (assert-map-like 1e3 empty-coll element-generator))
  ([n empty-coll key-generator value-generator]
     (assert-not-failed
       (quick-check n
         (prop/for-all [[a b actions] (gen-map-like empty-coll key-generator value-generator)]
           (assert-equivalent-maps a b)
           true)))))
