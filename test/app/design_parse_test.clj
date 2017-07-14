(ns app.design-parse-test
  (:use clojure.java.io)
  (:use clojure.test)
  (:use app.design-parse))

(deftest test-masking-parser-double-blind
  (is (= (parse-masking "Double Blind (Participant, Investigator, Outcomes Assessor)")
         ["Double Blind" "Participant" "Investigator", "Outcomes Assessor"]  )))

(deftest test-masking-parser-single-blind-single-spec
  (is (= (parse-masking "Single Blind (Outcomes Assessor)")
         ["Single Blind" "Outcomes Assessor"])))

(deftest test-masking-parser-single-blind-spec-only
  (is (= (parse-masking "Care provider")
         ["Single Blind" "Outcomes Assessor"])))

(deftest test-masking-parser-single-blind-spec-only
  (is (= (parse-masking "Outcomes Assessor")
         ["Single Blind" "Outcomes Assessor"])))

(deftest test-masking-parser-single-blind-no-spec
  (is (= (parse-masking "Single Blind")
         ["Single Blind"])))

(deftest test-masking-parser-no-blinding
  (is (= (parse-masking "Participant, Investigator")
    ["Double Blind" "Participant" "Investigator"])))

(deftest test-masking-double-blind-no-specs
  (is (= (parse-masking "Double-Blind")
         ["Double Blind"])))

(deftest test-no-masking
  (is (= (parse-masking "No masking")
         ["No masking"])))

(deftest test-parse-all-no-exceptions
  (is (nil? (with-open [rdr (reader "test/app/knownMaskings.txt")]
        (doseq [line (line-seq rdr)]
          (parse-masking line))))))
