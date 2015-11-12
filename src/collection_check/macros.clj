(ns collection-check.macros)

(defmacro clj-reporting-failing-actions [& body]
  `(let [old-report-fn# ~'clojure.test/report]
     (binding [clojure.test/report #(do (old-report-fn# %)
                                        (collection-check.core/report-failing-actions %))]
       ~@body)))

(defmacro cljs-reporting-failing-actions [& body]
  `(let [old-report-fn# ~'cljs.test/report]
     (binding [cljs.test/report #(do (old-report-fn# %)
                                     (collection-check.core/report-failing-actions %))]
       ~@body)))
