(ns app.design-parse-test
  (:use clojure.test)
  (:use app.design-parse))

(deftest test-masking-parser-double-blind
  (is (= (parse-masking "Double Blind (Participant, Investigator, Outcomes Assessor)")
         ["Double Blind" "Participant" "Investigator", "Outcomes Assessor"]  )))

(deftest test-masking-parser-single-blind-single-spec
  (is (= (parse-masking "Single Blind (Outcomes Assessor)")
         ["Single Blind" "Outcomes Assessor"])))

(deftest test-masking-parser-single-blind-single-spec
  (is (= (parse-masking "Outcomes Assessor")
         ["Single Blind" "Outcomes Assessor"])))


; (deftest test-masking-parser-no-blinding
;   (is (= (parse-masking "Participant, Investigator")
;     ["Participant" "Investigator"]))
;   )
