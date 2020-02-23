(ns backend-coding-challenge.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [org.httpkit.server :as srv]
            [compojure.core :as cmp]
            [compojure.route :as route]
            [ring.middleware.defaults :as ring]
            [clojure.data.json :as json])
  (:import [sun.misc Signal SignalHandler])
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

(defn round
  [x]
  (Float/parseFloat (format "%.2f" x)))

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
    (reduce max (into res (map (comp (partial * 1/2) (partial parser query)) alt-names)))))

(defn matches?
  [query entry]
  "Returns true if query matches entry (row of data)"
  (some #(s/includes? % query) (names entry)))

(defn result
  [[_ -name  _ _ lat lon _ _ country _ admin _ _ _ population & _]]
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
   :population population})

(defn total-score
  [{:keys [q latitude longitude]} res]
  (if (empty? res)
    []
    (let [scores (map (partial name-score q) res)
          ceil1000 (fn [x] (if (> 1000 x) x 1000))
          distances (if (and latitude longitude)
                      (map #(geo-distance latitude longitude (nth % 4) (nth % 5)) res)
                      (repeat (count res) 0))
          g-scores (->> distances (map ceil1000) (mapv #(/ (- 1100 %) 1100)))
          total-scores (map * scores g-scores)
          min-score (reduce min total-scores)
          max-score (reduce max total-scores)
          min-max (fn [x] (+ 0.1 (/ (* 0.9 (- x min-score)) (- max-score min-score))))]
      (mapv (comp round min-max) total-scores))))
 
(defn suggestions
  [req]
  (let [query (:q (:params req))
        lat (:latitude (:params req))
        lon (:longitude (:params req))]
   {:status 200
    :headers {"Content-Type" "application/json"}
    :body (if (empty? query)
            (json/write-str {:suggestions []})
            (let [query (s/trim (s/lower-case query))
                  param {:q query
                         :latitude (when lat (Float/parseFloat lat))
                         :longitude (when lon (Float/parseFloat lon))}
                  res (filterv (partial matches? query) DATA)
                  sug (mapv result res)
                  scores (total-score param res)]
              (json/write-str {:suggestions (->> scores 
                                                 (mapv #(assoc %1 :score %2) sug)
                                                 (sort-by :score)
                                                 reverse)})))}))

(cmp/defroutes cities
  (cmp/GET "/suggestions" []
           (ring/wrap-defaults suggestions ring/api-defaults)) 
  (route/not-found "Route not found. Use /suggestions"))

(defn -main
  [port]
  (let [http-srv (srv/run-server cities {:port (Integer/parseInt port)})]
    ;; Register a SIGTERM handler to gracefully shutdown the server
    (Signal/handle
      (new Signal "TERM")
      (reify SignalHandler
        (handle [this sig] ((fn [s] (http-srv) (System/exit 0)) sig))))))
