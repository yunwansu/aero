(ns aero.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn readers [profile]
  {'env (fn [x]
          (cond (vector? x) (or (System/getenv (str (first x))) (second x))
                :otherwise (System/getenv (str x))))
   'cond (fn [m]
           (cond (contains? m profile) (clojure.core/get m profile)
                 (contains? m :default) (clojure.core/get m :default)
                 :otherwise nil))})

(defn read-config
  ([r {:keys [profile]}]
   (edn/read
    {:readers (readers (or profile :default))}
    (java.io.PushbackReader. (io/reader r))))
  ([r]
   (read-config r {})))
