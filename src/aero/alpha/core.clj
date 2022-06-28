(ns aero.alpha.core)

(defn- queue [& xs]
  (into clojure.lang.PersistentQueue/EMPTY xs))

(defn- qu [coll]
  (apply queue coll))

(defn reassemble [this queue]
  ((get (meta this) `reassemble) this queue))

(defn kv-seq [x]
  (cond
    (and (map? x) (not (record? x)))
    (with-meta (into [] x) {`reassemble (fn [_ queue]
                                          (into (empty x) queue))})
    (set? x)
    (with-meta
      (map-indexed (fn [idx x] [idx x]) x)
      {`reassemble (fn [_ queue]
                     (into (empty x) (map second queue)))})
    (vector? x)
    (with-meta
      (map-indexed (fn [idx x] [idx x]) x)
      {`reassemble (fn [_ queue]
                     (into (empty x)
                           (mapv second (sort-by first queue))))})
    (seq? x)
    (with-meta (map-indexed (fn [idx x]) x)
      {`reassemble (fn [_ queue]
                     (with-meta
                       (apply list (map second (sort-by first queue)))
                       (meta x)))})
    :else
    nil))

(defmulti eval-tagged-literal
  (fn [tagged-literal opts env ks] (:tag tagged-literal)))

(declare expand)
(declare expand-coll)
(declare expand-scalar)

(defn expand-scalar [x opts env ks]
  (if (tagged-literal? x)
    (eval-tagged-literal x opts env (conj ks :form))
    {:aero.core/value x
     :aero.core/env (assoc env ks x)}))

(def ^:private ^:dynamic *max-skips* 1)
