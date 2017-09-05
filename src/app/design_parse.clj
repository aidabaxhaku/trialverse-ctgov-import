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
  (let [masking-types {"None (Open Label)" "Open"
                       "Single" "Single Blind"
                       "Double" "Double Blind"
                       "Triple" "Double Blind"
                       "Quadruple" "Double Blind"}]
    (masking-types the-key)))

(defn get-extra-terms [masking-map blinding]
  (case blinding
    "Single Blind" (if (> (count masking-map) 2)
                     (map second (rest (nth masking-map 2)))
                     `())
    "Double Blind" (if (> (count masking-map) 2)
                     (map second (rest (nth masking-map 2)))
                     `())
    "Open" `()))

(defn parse-masking [the-str]
  (if (= "N/A" the-str)
    "N/A"
    (let
      [masking-map (masking the-str)
       masking-spec (second masking-map)
       blinding (key-to-label (second masking-spec))
       extra-terms (get-extra-terms masking-map blinding)]
      (concat (list blinding) extra-terms))))

