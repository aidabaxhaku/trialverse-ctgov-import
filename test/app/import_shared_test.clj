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
