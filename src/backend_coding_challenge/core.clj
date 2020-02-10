(ns backend-coding-challenge.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:gen-class))

(def DATA
  (with-open [file (io/reader (io/resource "data/cities_canada-usa.tsv"))]
    (doall (csv/read-csv file :separator \tab :quote \`))))

(defn deg->rad
  [a]
  (/ (* a Math/PI) 180))

(defn geo-distance
  [lat1 long1 lat2 long2]
  "Returns the distance between two coordinates on Earth"
  (let [R-TERRA 6371] ;; Radius of Earth [km]
    (* R-TERRA
       (Math/acos
         (+ (* (Math/sin (deg->rad lat1))
               (Math/sin (deg->rad lat2)))
            (* (Math/cos (deg->rad lat1))
               (Math/cos (deg->rad lat2))
               (Math/cos (- (deg->rad long2)
                            (deg->rad long1)))))))))

(defn name-distance
  [name1 name2]
  "Returns the insert distance between two names"
  (if (s/includes? name2 name1)
    (count (s/replace-first name2 name1 ""))
    (when (s/includes? name1 name2)
      (count (s/replace-first name1 name2 "")))))

(defn name-score
  [query match]
  (- 1 (/ (name-distance query match)
          (count match))))

(defn names
  [entry]
  "Returns the normalized name list for row of data"
  (let [alt-names (s/split (->> entry (drop 3) first) #",")
        nalt-names (->> alt-names (map s/trim) (map s/lower-case))
        name (-> entry second s/trim s/lower-case)
        ascii (-> entry second s/trim s/lower-case)]
    (set (cons name (cons ascii nalt-names)))))
  
  
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
