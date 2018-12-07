(ns app.import-shared-test
  (:use clojure.test)
  (:use app.import-shared)
  (:require  [org.drugis.addis.rdf.trig :as trig]))

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

(deftest measurement-meta-rdf-test
  (is (= [[:uri "http://subject.com"]
          '([[:qname :ontology "of_outcome"] [:lit "outcome-uri"]]
            [[:qname :ontology "of_group"] [:lit "group-uri"]]
            [[:qname :ontology "of_moment"] [:lit "mm-uri"]])]
         (measurement-meta-rdf 
          [:uri "http://subject.com"]
          "outcome-uri" "group-uri" "mm-uri"))))

(deftest test-group-rdf
 (let [group-uri [:qname :instance "uuid"]
       expected-rdf [group-uri
                     '([[:qname :rdfs "label"] [:lit "title"]]
                       [[:qname :rdfs "comment"] [:lit "something"]]
                       [[:qname :rdf "type"] [:qname :ontology "Group"]])]]
   (is (= expected-rdf
          (group-rdf group-uri
                     {:title       "title"
                      :description "something"})))))

                  
(deftest test-mm-rdf
  (let [instance-uri [:qname :instance "uuid"]
        expected-rdf [instance-uri
                      '([[:qname :rdfs "label"] [:lit "title"]]
                        [[:qname :rdf "type"] [:qname :ontology "MeasurementMoment"]])]]
    (is (= expected-rdf
           (mm-rdf instance-uri "title")))))