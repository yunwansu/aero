(ns aero.reader
  (:require [clojure.string :refer [trim]]
            [clojure.java.shell :as sh]))

(defmulti reader (fn [opts tag value] tag))

(defmethod reader 'env [opts tag value]
  (cond (vector? value) (or (System/getenv (str (first value))) (second value))
        :otherwise (System/getenv (str value))))

(defmethod reader 'cond [{:keys [profile]} tag value]
  (cond (contains? value profile) (get value profile)
        (contains? value :default) (get value :default)
        :otherwise nil))

(defmethod reader 'hostname [{:keys [hostname]} tag value]
  (let [hostname (-> (sh/sh "hostname") :out trim)]
    (or (some (fn [[k v]]
                (when (or (= k hostname)
                          (and (set? k) (contains? k hostname)))
                  v))
              value)
        (get value :default))))
