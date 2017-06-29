(ns app.design-parse
  (:require [instaparse.core :as insta]))

; extracts blinding information from the ctgov masking string
(def masking
  (insta/parser
    "masking = noMasking | singleBlind | doubleBlind
     noMasking = 'No masking' | 'Open Label'
     singleBlind = 'Single Blind' | 'Single Blind ' <paropen> (who | unknown) <parclose> | who  
     doubleBlind = 'Double-Blind' | 'Double Blind ' <paropen> multipleWho <parclose>
     multipleWho = 'masked roles unspecified' | who <sep> (who <sep>?)+
     sep = ', '
     who = 'Care Provider' | 'Investigator' | 'Outcomes Assessor'
     unknown = 'masked role unspecified'"))
(defn parse-masking [the-str]
  (if (= "N/A" the-str)
    "N/A"
    (let
      [masking-map (masking the-str)
       blinding (second (second masking-map))
       extra-terms (map #(second %1) (rest (nth masking-map 2)))]

      (concat (list blinding) extra-terms))))

