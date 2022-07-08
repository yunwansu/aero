(ns aero.reader)

(defmulti reader (fn [opts tag value] tag))

(defmulti transform (fn [opts tag config-map] tag))
