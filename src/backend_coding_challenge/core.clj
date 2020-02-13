(ns backend-coding-challenge.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:gen-class))

(def DATA
  (with-open [file (io/reader (io/resource "data/cities_canada-usa.tsv"))]
    (let [data (csv/read-csv file :separator \tab :quote \`)]
      (doall
        (for [[id name ascii alt-names lat lon feat-class feat-code country cc2
               admin1 admin2 admin3 admin4 population elev dem tz mod-at] (rest data)]
          [(Integer/parseInt id)
           name
           ascii
           (s/split alt-names #",")
           (Float/parseFloat lat)
           (Float/parseFloat lon)
           (first feat-class)
           feat-code
           country
           cc2
           admin1
           admin2
           admin3
           admin4
           (Integer/parseInt population)
           (when (seq elev) (Integer/parseInt elev))
           (when (seq dem) (Integer/parseInt dem))
           tz
           mod-at]))))) 

(def CA-ADM
  {"01" "AB"
   "02" "BC"
   "03" "MB"
   "04" "NB"
   "05" "NL"
   "07" "NS"
   "08" "ON"
   "09" "PE"
   "10" "QC"
   "11" "SK"
   "12" "YT"
   "13" "NT"
   "14" "NU"})

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

(defn names
  [[_ name ascii alt-names & _]]
  "Returns the normalized name list for row of data"
  (into #{} (map (comp s/trim s/lower-case) (conj alt-names name ascii))))

(defn name-distance
  [query -name]
  "Returns the insert distance between a name and a query"
  (when (s/includes? -name query)
    (count (s/replace-first -name query ""))))

(defn name-score
  [query entry]
  (let [parser (fn [query -name]
                 (if-let [dist (name-distance query -name)]
                   (- 1 (/ dist (count -name)))
                   0))
        names (into #{} (map (comp s/trim s/lower-case) (subvec entry 1 3)))
        res (mapv (partial parser query) names)
        alt-names (into #{} (map (comp s/trim s/lower-case)) (entry 3))]
    (float (reduce max (into res (map (comp (partial * 1/2) (partial parser query)) alt-names))))))

(defn matches?
  [query entry]
  "Returns true if query matches entry (row of data)"
  (some #(s/includes? % query) (names entry)))

(defn result
  [query entry & coord]
  (let [[_ -name  _ _ lat lon _ _ country _ admin _ _ _ population & _] entry]
    {:name (str -name
                ", "
                (case country
                  "CA" (CA-ADM admin)
                  "US" admin)
                ", "
                (case country
                  "CA" "Canada"
                  "US" "USA"))
     :latitude lat
     :longitude lon
     :population population
     :score (name-score query entry)}))
              
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
