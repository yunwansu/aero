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

(defn expand-coll [x opts env ks]
  (let [steps (kv-seq x)]
    (loop [q (qu steps)
           ss []
           env env
           skip-count {}
           skipped #{}]
      (if-let [[k v :as item] (peek q)]
        (let [{k :aero.core/value
               k-incomplete? :aero.core/incomplete?
               env :aero.core/env
               :or {env env}
               :as k-expansion}
              (expand k opts env (conj ks k :aero.core/k))

              {:keys [aero.core/env aero.core/value aero.core/incomplete?]
               :or {env env}
               :as expansion}
              (when-not k-incomplete?
                (expand v opts env (conj ks k)))]
          (if (or k-incomplete? incomplete?)
            (if (<= *max-skips* (get skip-count item 0))
              (recur (pop q)
                     (conj ss [k value])
                     env
                     (update skip-count item (fnil inc 0))
                     (conj skipped (if k-incomplete?
                                     k-expansion
                                     expansion)))
              (recur (conj (pop q) [k value])
                     ss
                     env
                     (update skip-count item (fnil inc 0))
                     skipped))
            (recur (pop q)
                   (conj ss [k value])
                   (assoc env (conj ks k) value)
                   skip-count
                   skipped)))
        {:aero.core/value (reassemble steps ss)
         :aero.core/env env
         :aero.core/incomplete? (some #(>= % *max-skips*) (vals skip-count))
         :aero.core/incomplete (some :aero.core/incomplete skipped)
         :aero.core/_ss ss}))))

(defn expand [x opts env ks]
  (if (or (and (map? x) (not (record? x))) (set? x) (seq? x) (vector? x))
    (expand-coll x opts env ks)
    (expand-scalar x opts env ks)))

(defn expand-scalar-repeatedly [x opts env ks]
  (loop [x x]
    (let [x (expand-scalar x opts env ks)]
      (if (and (tagged-literal? (:aero.core/value x))
               (not (:aero.core/incomplete? x)))
        (recur (:aero.core/value x))
        x))))

(defn- expand-keys [m opts env ks]
  (loop [ks (keys m)
         m m]
    (if (seq ks)
      (let [{:keys [:aero.core/incomplete? :aero.core/value] :as expansion}
            (expand (first ks) opts env ks)]
        (if incomplete?
          (assoc expansion :aero.core/value (-> m
                                                (dissoc (first ks))
                                                (assoc value (get m (first ks)))))
          (recur (rest ks) (-> m
                               (dissoc (first ks))
                               (assoc value (get m (first ks)))))))
      {:aero.core/value m})))

(defn- expand-set-keys [m]
  (reduce-kv
   (fn [m k v]
     (if (set? k)
       (reduce #(assoc %1 %2 v) m k)
       (assoc m k v))) {} m))

(defn- rewrap [tl]
  (fn [v]
    (tagged-literal (:tag tl) v)))

(defn expand-case [case-value tl opts env ks]
  (let [{m-incomplete? :aero.core/incomplete?
         m :aero.core/value
         :as m-expansion}
        (expand-scalar-repeatedly (:form tl) opts env ks)
        {ks-incomplete? :aero.core/incomplete?
         :keys [:aero.core/value] :as ks-expansion}
        (when-not m-incomplete?
          (expand-keys m opts env ks))]
    (if (or m-incomplete? ks-incomplete?)
      (update (or m-expansion ks-expansion) :aero.core/value (rewrap tl))
      (let [set-keys-expanded (expand-set-keys value)]
        (expand (get set-keys-expanded case-value
                     (get set-keys expanded :default))
                opts env ks)))))
