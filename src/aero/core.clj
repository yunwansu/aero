(ns aero.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :refer [trim]]
            [clojure.java.shell :as sh]
            [clojure.walk :refer [postwalk]]
            [clojure.pprint :refer [pprint]]
            [aero.reader :refer [reader transform]]))

(declare read-config)

(defmethod reader 'env [opts tag value]
  (cond (vector? value) (or (System/getenv (str (first value))) (second value))
        :otherwise (System/getenv (str value))))

(defmethod reader 'envf [opts tag [fmt & args]]
  (apply format fmt (map (partial reader nil 'env) args)))

(defmethod reader 'format [opts tag [fmt & args]]
  (apply format fmt args))

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

(defmethod reader 'file [opts tag value]
  (read-config value opts))

(defmethod reader 'path [opts tag value]
  (with-meta value {::tag 'path}))

(defmethod reader :default [_ tag value]
  (if tag
    (try (with-meta value {::tag tag})
         (catch ClassCastException e value))
    value))

(defmethod transform 'path [opts tag config-map]
  (postwalk (fn [v]
              (if (= 'path (::tag (meta v)))
                (get-in config-map v)
                v))
            config-map))

(defmethod transform :default [opts tag config-map]
  config-map)


  
(defn read-config
  ([r {:keys [transforms] :as opts}]
   (let [default-profile {:profile :default}
         config-file-path {:filepath (str r)}
         default-reader (->> (merge default-profile config-file-path opts) (partial reader))
         config (with-open [pr (java.io.PushbackReader. (io/reader r))]
                  (edn/read {:eof nil
                             :default default-reader}
                            pr))]
     (reduce (fn [acc tag]
               (transform opts
                          ((comp symbol name) tag)
                          acc)) config transforms)))
  ([r]
   (read-config r {})))
