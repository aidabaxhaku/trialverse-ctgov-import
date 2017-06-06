(ns app.design-parse
  (:require [instaparse.core :as insta]))

; extracts blinding information from the ctgov masking string
(def masking
  (insta/parser
    "masking = blindingtype? <space>? specs
     blindingtype = #'[\\w -]*\\w'
     space = ' '
     specs = <paropen>? val(<sep> val)* <parclose>?
     paropen = '('
     parclose = ')'
     sep = ', '
     val = #'[\\w/ -]*\\w'"))
(defn parse-masking [the-str]
  (if (= "N/A" the-str)
    "N/A"
    (let
      [masking-map (masking the-str)
       _dummy (println masking-map)
       blinding (second (second masking-map))
       extra-terms (map #(second %1) (rest (nth masking-map 2)))]

      (concat (list blinding) extra-terms))))

