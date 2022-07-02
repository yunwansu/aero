(ns aero.core
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [aero.reader :refer [reader]]))

(defn read-config
  ([r opts]
   (let [default-profile {:profile :default}
         config-file-path {:filepath (str r)}
         default-reader (->> (merge default-profile config-file-path opts) (partial reader))]
     (with-open [pr (java.io.PushbackReader. (io/reader r))]
       (edn/read {:eof nil
                  :default default-reader}
                 pr))))
  ([r]
   (read-config r {})))
