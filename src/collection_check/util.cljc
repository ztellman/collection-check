(ns collection-check.util)

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

(def report-sym
  #?(:clj  'clojure.test/report
     :cljs 'cljs.test/report)
  )

;; (defn report-bindings []
;;   #?(:clj ['clojure.test/report #(do (clojure.test/report %)
;;                                   (report-failing-actions %))]
;;      :cljs ['cljs.test/report #(do (cljs.test/report %)
;;                                   (report-failing-actions %))]))

;; #?(:clj
;;    (defmacro reporting-failing-actions [& body]
;;      `(binding ~(report-bindings)
;;           ~@body)))

;; (defmacro reporting-failing-actions [& body]
;;   `(binding [#?@(:clj [clojure.test/report #(do (clojure.test/report %)
;;                                                 (report-failing-actions %))]
;;                  :cljs [cljs.test/report #(do (cljs.test/report %)
;;                                               (report-failing-actions %))])]
;;      ~@body))

;; (macroexpand '(clj-reporting-failing-actions (println 1)))

(defmacro clj-reporting-failing-actions [& body]
  `(let [old-report-fn# ~'clojure.test/report]
     (binding [clojure.test/report #(do (old-report-fn# %)
                                        (report-failing-actions %))]
       ~@body)))

(defmacro cljs-reporting-failing-actions [& body]
  `(let [old-report-fn# ~'cljs.test/report]
     (binding [cljs.test/report #(do (old-report-fn# %)
                                     (report-failing-actions %))]
       ~@body)))
