(ns app.design-parse
  (:require [instaparse.core :as insta]))

; extracts blinding information from the ctgov masking string
(def masking
  (insta/parser
    "masking = maskingType maskingDetails?
     maskingType = 'None (Open Label)' | 'Single' | 'Double' | 'Triple' | 'Quadruple'
     maskingDetails = <space paropen> (who <sep>?)+ <parclose>
     sep = ', '
     space = ' '
     paropen = '('
     parclose = ')'
     who = 'Care Provider' | 'Investigator' | 'Outcomes Assessor' | 'Participant'"))

(defn key-to-label [the-key]
  (let [masking-types {"None (Open Label)" "Open Label"
                       "Single" "Single Blind"
                       "Double" "Double Blind"
                       "Triple" "Triple Blind"
                       "Quadruple" "Double Blind"}]
    (if (nil? (masking-types the-key)) (println (str "unknown masking type: " the-key))
    (masking-types the-key)))

(defn extra-terms [masking-map] 
  (if (> (count masking-map) 2)
    (map second (rest (nth masking-map 2)))
    `()))

(defn get-extra-terms [masking-map blinding]
  (case blinding
    "Single Blind" (extra-terms masking-map)
    "Double Blind" (extra-terms masking-map)
    "Triple Blind" (extra-terms masking-map)
    "Open Label" `()))

(defn parse-masking [the-str]
  (if (= "N/A" the-str)
    "N/A"
    (let
      [masking-map (masking the-str)
       masking-spec (second masking-map)
       blinding (key-to-label (second masking-spec))
       extra-terms (get-extra-terms masking-map blinding)]
      (concat (list blinding) extra-terms))))

