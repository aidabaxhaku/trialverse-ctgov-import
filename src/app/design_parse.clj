(ns app.design-parse
  (:require [instaparse.core :as insta]))

; extracts blinding information from the ctgov masking string
(def masking
  (insta/parser
    "masking = noMasking | singleBlind | doubleBlind
     noMasking = 'No masking' | 'Open Label'
     singleBlind = <'Single Blind'> | <'Single Blind'> <space>+ <paropen> (who | unknown) <parclose> | who
     doubleBlind = <'Double-Blind'> | <'Double Blind'> <space>+ <paropen> multipleWho <parclose> | multipleWho
     multipleWho = 'masked roles unspecified' | who <sep> (who <sep>?)+
     sep = ', '
     space = ' '
     paropen = '('
     parclose = ')'
     who = 'Care Provider' | 'Investigator' | 'Outcomes Assessor' | 'Participant'
     unknown = 'masked role unspecified'"))

(defn key-to-label [the-key]
  (let [masking-types {:singleBlind "Single Blind"
                       :doubleBlind "Double Blind"
                       :noMasking "No masking"}]
    (the-key masking-types)))

(defn get-extra-terms [masking-spec blinding]
  (case blinding
    "Single Blind" (if (> (count masking-spec) 1)
                     (rest (second masking-spec))
                     `())
    "Double Blind" (if (> (count masking-spec) 1)
                     (map second (rest (nth masking-spec 1)))
                     `())
    "No masking" `()))

(defn parse-masking [the-str]
  (if (= "N/A" the-str)
    "N/A"
    (let
      [masking-map (masking the-str)
       masking-spec (second masking-map)
       blinding (key-to-label (first masking-spec))
       extra-terms (get-extra-terms masking-spec blinding)]
      (concat (list blinding) extra-terms))))

