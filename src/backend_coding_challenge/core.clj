(ns backend-coding-challenge.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:gen-class))

(def data
  (with-open [file (io/reader (io/resource "data/cities_canada-usa.tsv"))]
    (doall (csv/read-csv file))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
