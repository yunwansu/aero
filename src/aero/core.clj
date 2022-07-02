(ns aero.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :refer [trim]]
            [clojure.java.shell :as sh]))

(defn readers [{:keys [profile hostname]}]
  {'env (fn [x]
          (cond (vector? x) (or (System/getenv (str (first x))) (second x))
                :otherwise (System/getenv (str x))))
   'cond (fn [m]
           (cond (contains? m profile) (clojure.core/get m profile)
                 (contains? m :default) (clojure.core/get m :default)
                 :otherwise nil))
   'hostname (fn [m]
               (or (some (fn [[k v]]
                           (when (or (= k hostname)
                                     (and (set? k) (contains? k hostname)))
                             v))
                         m)
                   (get m :default)))})

(defn read-config
  ([r {:keys [profile]}]
   (let [hostname (-> (sh/sh "hostname") :out trim)]
     (edn/read
      {:readers (readers {:profile (or profile :default)
                          :hostname hostname})}
      (java.io.PushbackReader. (io/reader r)))))
  ([r]
   (read-config r {})))
