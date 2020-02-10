(ns backend-coding-challenge.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:gen-class))

(def data
  (with-open [file (io/reader (io/resource "data/cities_canada-usa.tsv"))]
    (doall (csv/read-csv file :separator \tab :quote \`))))

(defn deg->rad
  [a]
  (/ (* a Math/PI) 180))

(defn geo-distance
  [lat1 long1 lat2 long2]
  (let [R-TERRA 6371] ;; Radius of Earth [km]
    (* R-TERRA
       (Math/acos
         (+ (* (Math/sin (deg->rad lat1))
               (Math/sin (deg->rad lat2)))
            (* (Math/cos (deg->rad lat1))
               (Math/cos (deg->rad lat2))
               (Math/cos (- (deg->rad long2)
                            (deg->rad long1)))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
