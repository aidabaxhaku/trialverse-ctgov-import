(ns app.design-parse-test
  (:use clojure.java.io)
  (:use clojure.test)
  (:use app.design-parse))

(deftest test-masking-parser-double-blind
  (is (= (parse-masking "Double (Participant, Investigator)")
         ["Double Blind" "Participant" "Investigator"]  )))

(deftest test-masking-parser-single-blind-single-spec
  (is (= (parse-masking "Single (Outcomes Assessor)")
         ["Single Blind" "Outcomes Assessor"])))

(deftest test-masking-parser-single-blind-no-spec
  (is (= (parse-masking "Single")
         ["Single Blind"])))

(deftest test-masking-double-blind-no-specs
  (is (= (parse-masking "Double")
         ["Double Blind"])))

(deftest test-no-masking
  (is (= (parse-masking "None (Open Label)")
         ["Open Label"])))

(deftest test-parse-all-no-exceptions
  (is (nil? (with-open [rdr (reader "test/app/knownMaskings.txt")]
        (doseq [line (line-seq rdr)]
          (parse-masking line))))))
