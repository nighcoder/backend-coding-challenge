(ns backend-coding-challenge.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s])
  (:gen-class))

(def DATA
  (with-open [file (io/reader (io/resource "data/cities_canada-usa.tsv"))]
    (doall (csv/read-csv file :separator \tab :quote \`))))

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

(defn name-min-distance
  [query names]
  "Returns a pair of name which has the minimum distance from query, along with that distance value"
  (loop [names names
         res-name nil
         res-dist nil]
    (if (seq names)
      (let [name (first names)
            new-dist (count (s/replace-first name query ""))]
        (if (and (s/includes? name query)
                 (or (not res-dist)
                     (> res-dist new-dist)))
          ;; If query is substring of name AND EITHER we have no distance result OR the new distance result is
          ;; smaller than the previous value recur with new values
          (recur (rest names) name new-dist)
          ;; If query is not a substring of name OR it is one, but the new distance is bigger than the previous one
          ;; recur with the old values
          (recur (rest names) res-name res-dist)))
      (list res-name res-dist))))

;; Obsolete after defining name-min-distance
;(defn name-distance
;  [name1 name2]
;  "Returns the insert distance between two names"
;  (if (s/includes? name2 name1)
;    (count (s/replace-first name2 name1 ""))
;    (when (s/includes? name1 name2)
;      (count (s/replace-first name1 name2 "")))))

(defn name-score
  [[name distance]]
  (- 1 (/ distance (count name))))

(defn names
  [entry]
  "Returns the normalized name list for row of data"
  (let [alt-names (->> entry (drop 3) first)
        nalt-names (when (seq alt-names)
                     (->> (s/split alt-names #",") (map s/trim) (map s/lower-case)))
        name (-> entry second s/trim s/lower-case)
        ascii (->> entry (drop 2) first s/trim s/lower-case)]
    (->> nalt-names
         (cons ascii)
         (cons name)
         set)))

(defn country
  [entry]
  "Returns the country name in entry"
  (let [code (->> entry (drop 8) first)]
    (case code
      "CA" "Canada"
      "US" "USA")))

(defn admin
  [entry]
  "Returns the ISO code for administrative region of country"
  (let [code (->> entry (drop 10) first)]
    (case (country entry)
      "Canada" (CA-ADM code)
      "USA" code)))
 
(defn matches?
  [query entry]
  "Returns true if query matches entry (row of data)"
  (some #(s/includes? % query) (names entry)))

(defn result
  [query entry & coord]
  {:name (str (second entry)
              ", "
              (admin entry)
              ", "
              (country entry))
   :latitude (->> entry (drop 4) first)
   :longitude (->> entry (drop 5) first)
   :population (->> entry (drop 14) first)
   :score (name-score query (names entry))})
              
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
