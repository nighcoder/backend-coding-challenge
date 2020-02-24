(ns backend-coding-challenge.core-test
  (:require [clojure.test :refer :all]
            [backend-coding-challenge.core :refer :all]))

(defn latitude?
  [x]
  "Returns true if x is valid latitude coordinate"
  (>= 90 x -90))

(defn longitude?
  [x]
  "Returns true if x is valid longitude coordinate"
  (>= 180 x -180))

(defn rel-err
  [x y]
  (/ (- x y) x))

(deftest DATA-test
  (is (= 7237 (count DATA)))
  (is (= 7237 (count (set (map first DATA)))))      ;; Every ID is unique.
  (is (not-any? empty? (map second DATA)))          ;; DATA does not contain a single empty name
  (is (every? latitude? (map #(nth % 4) DATA)))
  (is (every? longitude? (map #(nth % 5) DATA)))
  (is (every? #{"US" "CA"} (map #(nth % 8) DATA))))

(deftest convs
  (is (= (deg->rad 30) (/ Math/PI 6)))
  (is (= (deg->rad 60) (/ Math/PI 3)))
  (is (= (deg->rad 180) Math/PI))
  (is (= (round (/ Math/PI 6)) (float 0.52)))
  (is (= (round (/ Math/PI 3)) (float 1.05)))
  (is (= (round 0.999) (round 1.001) 1.0)))

(deftest queries
  (let [c0 (nth DATA 6956) ;; Seattle
        c1 (nth DATA 598) ;; WashingtonDC
        c2 (nth DATA 335) ;; Thunder-Bay
        c3 (nth DATA 381) ;; Sydney
        c4 (nth DATA 343) ;; Vancouver
        c5 (nth DATA 1900) ;; St. Louis
        c6 (nth DATA 2779) ;; Hewitt
        q0 {:q "sia"}
        q1 {:q "vanc"}
        q2 {:q "gorad" :latitude 46.879433 :longitude -113.997136}
        r0 (filterv (partial matches? "vanc") DATA)
        r1 (filterv (partial matches? "gorad") DATA)]
    ;; Checking if geo-distance result is within 1% relative error from the reference distances (taken from google maps)
    (is (< (rel-err 191.8 (geo-distance (c0 4) (c0 5) (c4 4) (c4 5)))
           0.01))
    (is (< (rel-err 4597.5 (geo-distance (c0 4) (c0 5) (c3 4) (c3 5)))
           0.01))
    (is (< (rel-err 1087.35 (geo-distance (c2 4) (c2 5) (c5 4) (c5 5)))
           0.01))
    (is (< (rel-err 1140.57 (geo-distance (c1 4) (c1 5) (c5 4) (c5 5)))
           0.01))
    ;; Checking the names set
    (is (= #{"തണ്ടർ ബേ" "선더베이" "ثاندر باي، أونتاريو" "桑德贝" "tander bėjus" "тандер-бей" "サンダーベイ" "тандер беј"
             "thunder bay" "تھنڈربے، انٹاریو" "تھنڈر بے" "sang de bei" "thandr bay" "tander-bej" "tander bejus" "seondeobei"
             "tander bej" "sandabei" "yqt" "awntaryw"}
           (names c2)))
    (is (= #{"sejnt luis" "saint louis" "sent-lüis" "sant lwys" "ಸೈಂಟ್ ಲೂಯಿಸ್" "სენტ-ლუისი" "san luis" "gorad sent-luis"
             "செயின்ட் லூயிஸ்" "senthluys" "sent luis" "snt lwyys" "సెయింట్ లూయిస్" "sentoruisu" "seyint luyis" "세인트루이스"
             "saint luyis" "sent luisas" "سانت لويس، ميزوري" "сент-луис" "सेंट लुईस" "سنت لوئیس" "st louis" "myzwry" "сент-луїс"
             "เซนต์หลุยส์" "горад сент-луіс" "ceyint luyis" "sent-luis" "סנט לואיס" "сент луис" "st. louis" "stl" "圣路易斯"
             "sankta luiso" "セントルイス" "sheng lu yi si" "urbs sancti ludovici" "senta lu'isa" "sent-lueis" "seinteulu-iseu"
             "сейнт луис" "sentluisa"}
           (names c5)))
    (is (= #{"hewitt"}
           (names c6)))
    ;; Checking the result fn
    (is (= {:name "Washington, D. C., DC, USA", :latitude (float 38.89511), :longitude (float -77.03637), :population 601723}
           (result c1)))
    (is (= {:name "Sydney, NS, Canada", :latitude (float 46.1351), :longitude (float -60.1831), :population 105968}
           (result c3)))
    (is (= {:name "St. Louis, MO, USA", :latitude (float 38.62727), :longitude (float -90.19789), :population 319294}
           (result c5)))
    ;; Checking name-score fn
    (is (= 3/10 (name-score (:q q0) c0)))
    (is (= 4/9 (name-score (:q q1) c4)))
    ;; Checking the matching function
    (is (matches? (:q q0) c0))
    (is (not (matches? (:q q0) c3)))
    (is (matches? (:q q1) c4))
    (is (not (matches? (:q q1) c6)))
    (is (= 5 (count r0)))
    (is (= 126 (count r1)))
    ;; Checking total-score fn
    (is (= (mapv float [0.1 1.0 0.2 1.0 1.0])
           (total-score q1 r0)))
    (is (= (mapv float [0.14 0.68 0.35 0.11 0.15 0.13 0.13 0.14 0.13 0.12 0.13 0.16 0.29 0.12 0.12 0.14 0.33 0.13
                        0.11 0.14 0.12 0.39 0.13 0.14 0.13 0.12 0.15 0.12 0.13 0.12 0.15 0.13 0.16 0.14 0.15 0.12
                        0.11 0.13 0.15 0.14 0.13 0.14 0.12 0.13 0.13 0.13 0.13 0.13 0.11 0.11 0.13 0.15 0.16 0.11
                        0.16 0.12 0.13 0.12 0.14 0.13 0.1 0.13 0.13 0.14 0.16 0.13 0.15 0.13 0.15 0.13 0.13 0.11
                        0.14 0.12 0.13 0.13 0.14 0.14 0.13 0.15 0.15 0.13 0.13 0.13 0.15 0.13 0.13 0.14 0.13 0.12
                        0.14 0.12 0.13 0.15 0.13 0.14 0.13 0.13 0.11 0.15 0.12 0.13 0.11 0.12 0.13 0.11 0.1 0.14
                        0.14 0.12 0.13 0.13 0.15 0.16 1.0 0.14 0.37 0.43 0.19 0.29 0.55 0.95 0.54 0.19 0.13 0.13])
           (total-score q2 r1)))))
