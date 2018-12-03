(ns app.import-shared-test
  (:use clojure.test)
  (:use app.import-shared))

(deftest test-sort-equivalent-values
  (let [[sorted-uris sorted-info] (sort-equivalent-values
                                    { :a "unique-value" 
                                      :b "duplicate-value" 
                                      :c "duplicate-value"}
                                    identity)]
    ; can't do easy object equality due to random uris, so per-case inspection
    (is (= 3 (count sorted-uris)))
    (is (= (:b sorted-uris) (:c sorted-uris)))
    (is (= 2 (count sorted-info)))
    (is (= "unique-value" (sorted-info (:a sorted-uris))))
    (is (= "duplicate-value" (sorted-info (:b sorted-uris))))
  ))

(deftest test-starts-with-any
  (let [input "hEllo this is a string"]
    (is (string-starts-with-any? input ["hEllo"]))
    (is (not (string-starts-with-any? input ["case-sensitive" "Hello"])))))

(deftest test-build-uris-of-type
  (let [uris (build-uris-of-type (range 1 9) :outcome)]
    (is (= 8 (count uris))
        (is (same-ignoring-order?
             (map #(vector %1 %2) (repeat :outcome) (range 1 9))
             (keys uris))))))
