(ns app.eudract-import-test
  (:require [riveted.core :as vtd]
            [org.drugis.addis.rdf.trig :as trig])
  (:use clojure.test)
  (:use app.eudract-import))

(def xml (vtd/navigator (slurp "test/app/eudract.xml")))
(def hba1c-change-xml (first (vtd/search xml "/result/endPoints/endPoint")))
(defn same-ignoring-order? [coll1 coll2]
  (= (set coll1)
     (set coll2)))

(defn outcomes-one-through-x [x]
  (map #(vector :outcome %) (range 1 (+ x 1))))

(deftest test-find-eudract-id
  (is (= "2013-004502-26"
         (get-eudract-number xml))))

(deftest test-find-nct-id
  (is (= "NCT02305381"
         (get-nct-id xml))))

(deftest test-build-registration
  (let [eudract-id "2013-004502-26"
        reg-uri (trig/iri :ictrp eudract-id)
        built-registration (build-registration reg-uri eudract-id)]
    (is (= [reg-uri '([[:qname :ontology "registry"]
                       [:uri "http://trials.drugis.org/registries#EudraCT"]]
                      [[:qname :ontology "registration_id"]
                       [:lit "2013-004502-26"]]
                      [[:qname :bibo "uri"]
                       [:lit "https://www.clinicaltrialsregister.eu/ctr-search/trial/2013-004502-26/results"]])]
           built-registration))))

(deftest test-find-measurement-moments
  (let [[found-mm-uris found-mm-info] (find-measurement-moments xml)]
    (is (same-ignoring-order?
         (concat (outcomes-one-through-x 8) '([:baseline] [:events]))
         (keys found-mm-uris)))
    (is (=
         '("From baseline to week 30"
           "Baseline"
           "From the first dose of trial product until the end of the post-treatment follow-up period.The follow-up visit was scheduled to take place 5 weeks after the date of last dose of trial product with a visit window of +7 days (maximum 36 weeks)."
           "After 30 weeks treatment"
           "After 30 weeks of treatment")
         (vals found-mm-info)))))

(deftest test-outcome-xml-findable 
  (is (= 8
         (count (vtd/search xml "/result/endPoints/endPoint")))))

(deftest test-build-outcome-uris
  (let [outcome-uris (build-outcome-uris (vtd/search xml "/result/endPoints/endPoint"))]
    (is (= 8 (count outcome-uris))
        (is (same-ignoring-order? 
             (outcomes-one-through-x 8)
             (keys outcome-uris))))))

(deftest test-outcome-measurement-properties-non-categorical
  (let [found-measurement-properties (outcome-measurement-properties hba1c-change-xml)]
    (is (= {:simple     true
            :is-count?   false
            :categories ()
            :param      "MEASURE_TYPE.leastSquares"
            :dispersion "ENDPOINT_DISPERSION.standardError"
            :units      "percentage of glycosylated hemoglobin"}
           found-measurement-properties))))
(deftest test-outcome-results-properties-non-categorical
  
  )
; prereq: outcome-measurement-properties
; prereq: outcome-results-properties
; (deftest test-outcome-rdf
;   (let [outcome-uris (build-outcome-uris (vtd/search xml "/result/endPoints/endPoint"))
;         [mm-uris mm-info] (find-measurement-moments xml)
;         generated-rdf (outcome-rdf xml 1 outcome-uris mm-uris)]

;        )]))
