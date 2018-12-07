(ns app.eudract-import-test
  (:require [riveted.core :as vtd]
            [app.import-shared :as lib]
             [clojure.set :refer [difference]]
  
            [org.drugis.addis.rdf.trig :as trig])
  (:use clojure.test)
  (:use app.eudract-import))

(def xml (vtd/navigator (slurp "test/app/eudract.xml")))
(def hba1c-change-xml (first (vtd/search xml "/result/endPoints/endPoint")))
(def hba1c-under-7-percent (nth (vtd/search xml "/result/endPoints/endPoint") 
                           6))
(def age-categorical (vtd/at xml "/result/baselineCharacteristics/ageCategoricalCharacteristic"))
(def age-continuous (vtd/at xml "/result/baselineCharacteristics/ageContinuousCharacteristic"))
(def decreased-appetite (first (find-adverse-events xml)))

(def arm-ids '("arm1Id" "arm2Id" "arm3Id"))
(def baseline-group-ids '("baselineGroup1Id" "baselineGroup2Id" "baselineGroup3Id"))
(def adverse-event-group-ids '("ReportingGroup-1" "ReportingGroup-2" "ReportingGroup-3"))
(def group-ids (concat arm-ids adverse-event-group-ids baseline-group-ids))
                
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
  (let [[found-mm-ids found-mm-info] (find-measurement-moments xml)]
    (is (lib/same-ignoring-order?
         (concat (outcomes-one-through-x 8) '([:baseline] [:events]))
         (keys found-mm-ids)))
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


(def hba1c-properties {:simple     true
                       :is-count?  false
                       :categories ()
                       :param      "MEASURE_TYPE.leastSquares"
                       :dispersion "ENDPOINT_DISPERSION.standardError"
                       :units      "percentage of glycosylated hemoglobin"})
(def hba1c-under-7-percent-properties {:simple     true
                                       :is-count?  false
                                       :categories ()
                                       :param      "MEASURE_TYPE.number"
                                       :dispersion "ENDPOINT_DISPERSION.na"
                                       :units      "percentage of subjects"})

(deftest test-outcome-measurement-properties
  (let [found-properties (outcome-measurement-properties hba1c-change-xml)
        expected-properties hba1c-properties]
    (is (= expected-properties found-properties))))

(deftest test-outcome-measurement-properties-number
  (let [found-properties (outcome-measurement-properties hba1c-under-7-percent)
        expected-properties hba1c-under-7-percent-properties]
    (is (= expected-properties found-properties))))

(deftest test-outcome-results-properties-continuous
  (is (= '(["least_squares_mean" "value"] ["standard_error" "spread"])
         (outcome-results-properties hba1c-properties))))

; (deftest test-outcome-measurement-properties-categorical
;   (is (= )))

(deftest test-outcome-results-properties-dichotomous
  (is (= '(["percentage" "value"])
         (outcome-results-properties hba1c-under-7-percent-properties))))

(deftest test-outcome-rdf-least-squares
  (let [outcome-uris        {[:outcome 1] [:qname :instance "outcome-uri"]}
        mm-uris             {[:outcome 1] [:qname :instance "mm-uri"]}
        generated-rdf       (outcome-rdf hba1c-change-xml 1 outcome-uris mm-uris)
        expected-properties '([[:qname :rdf "type"] [:qname :ontology "Endpoint"]]
                              [[:qname :rdfs "label"] [:lit "Change in HbA1c"]] 
                              [[:qname :rdfs "comment"] [:lit "Estimated mean change from baseline in HbA1c at week 30. The post-baseline responses are analysed using a mixed model for repeated measurements with treatment, country and stratification variable (HbA1c level at screening [<= 8.0% or > 8.0%] crossed with use of metformin [yes or no]; 2 by 2 levels) as fixed factors and baseline value as covariate, all nested within visit. Mean estimates are adjusted according to observed baseline distribution. Missing data was imputed using mixed model for repeated measurements. Analysis was performed on full analysis set which included all randomised subjects who had received at least 1 dose of randomised semaglutide or placebo."]] 
                              [[:qname :ontology "is_measured_at"] [:qname :instance "mm-uri"]] 
                              [[:qname :ontology "has_result_property"] [:qname :ontology "sample_size"]]
                              [[:qname :ontology "of_variable"]
                               [:blank ([[:qname :ontology "measurementType"]
                                         [:qname :ontology "continuous"]])]]
                              [[:qname :ontology "has_result_property"] [:qname :ontology "least_squares_mean"]] 
                              [[:qname :ontology "has_result_property"] [:qname :ontology "standard_error"]])]
    (is (every? true? (map = expected-properties (second generated-rdf))))))

(deftest test-outcome-rdf-number
  (let [outcome-uris        {[:outcome 1] [:qname :instance "outcome-uri"]}
        mm-uris             {[:outcome 1] [:qname :instance "mm-uri"]}
        generated-rdf       (outcome-rdf hba1c-under-7-percent 1 outcome-uris mm-uris)
        expected-properties '([[:qname :rdf "type"] [:qname :ontology "Endpoint"]]
                              [[:qname :rdfs "label"]  [:lit "HbA1c below 7.0%"]]
                              [[:qname :rdfs "comment"] [:lit "Percentage of subjects with HbA1C below 7.0%"]]
                              [[:qname :ontology "is_measured_at"] [:qname :instance "mm-uri"]]
                              [[:qname :ontology "has_result_property"] [:qname :ontology "sample_size"]]
                              [[:qname :ontology "of_variable"]
                               [:blank ([[:qname :ontology "measurementType"]
                                         [:qname :ontology "dichotomous"]])]]
                              [[:qname :ontology "has_result_property"] [:qname :ontology "percentage"]])]
    (is (every? true? (map = expected-properties (second generated-rdf))))))

(deftest test-find-adverse-events
  (is (= 45 (count (find-adverse-events xml)))))

(deftest test-adverse-event-rdf-nonserious
  (let [outcome-uris        {[:event 1] [:qname :instance "outcome-uri"]}
        mm-uris             {[:events] [:qname :instance "mm-uri"]}
        expected-rdf-properties '([[:qname :rdf "type"] [:qname :ontology "AdverseEvent"]]
                                  [[:qname :rdfs "label"] [:lit "Decreased appetite"]]
                                  [[:qname :ontology "is_serious"] [:lit false]]
                                  [[:qname :rdfs "comment"] [:lit "Decreased appetite"]]
                                  [[:qname :ontology "is_measured_at"] [:qname :instance "mm-uri"]]
                                  [[:qname :ontology "of_variable"]
                                   [:blank ([[:qname :ontology "measurementType"]
                                             [:qname :ontology "dichotomous"]])]]
                                  [[:qname :ontology "has_result_property"] [:qname :ontology "sample_size"]]
                                  [[:qname :ontology "has_result_property"] [:qname :ontology "count"]]
                                  [[:qname :ontology "has_result_property"] [:qname :ontology "event_count"]])]
     (is (= expected-rdf-properties
            (second
             (adverse-event-rdf decreased-appetite 
                                1 
                                outcome-uris 
                                mm-uris))))))

(deftest test-find-baseline-xml
  (let [ baseline-xml (find-baseline-xml xml)]
    (is (= 10 (+ 
               (count (:continuous baseline-xml))
               (count (:categorical baseline-xml)))))))

(deftest test-baseline-var-rdf-continuous
  (let [baseline-uris           {[:baseline 1] [:qname :instance "baseline-uri"]}
        mm-uris                 {[:baseline] [:qname :instance "mm-uri"]}
        expected-rdf-properties '([[:qname :rdf "type"]
                                   [:qname :ontology "PopulationCharacteristic"]]
                                  [[:qname :rdfs "label"] [:lit "Age Continuous"]]
                                  [[:qname :ontology "is_measured_at"]
                                   [:qname :instance "mm-uri"]]
                                  [[:qname :ontology "of_variable"]
                                   [:blank ([[:qname :ontology "measurementType"]
                                             [:qname :ontology "continuous"]])]]
                                  [[:qname :ontology "has_result_property"]
                                   [:qname :ontology "mean"]]
                                  [[:qname :ontology "has_result_property"]
                                   [:qname :ontology "standard_deviation"]])]
    (is (= expected-rdf-properties
           (second (baseline-var-rdf-continuous age-continuous 1 baseline-uris mm-uris))))))

(deftest test-baseline-var-rdf-categorical
  (let [baseline-uris           {[:baseline 1] [:qname :instance "baseline-uri"]}
        mm-uris                 {[:baseline] [:qname :instance "mm-uri"]}
        expected-rdf-properties '([[:qname :rdf "type"]
                                   [:qname :ontology "PopulationCharacteristic"]]
                                  [[:qname :rdfs "label"] [:lit "Age Categorical"]]
                                  [[:qname :ontology "is_measured_at"]
                                   [:qname :instance "mm-uri"]]
                                  [[:qname :ontology "of_variable"]
                                   [:blank ([[:qname :ontology "measurementType"]
                                             [:qname :ontology "categorical"]])]])
        found-baseline-rdf      (second
                                 (baseline-var-rdf-categorical
                                  age-categorical 1 baseline-uris mm-uris))]
    (is (= expected-rdf-properties
           found-baseline-rdf))))

(deftest test-get-categories-for-variable
  (let [categories      (get-categories-for-variable age-categorical)
        expected-ids    '("_efb3e754-0e32-43e7-ab08-b3580c330700"
                          "_b34cc9cf-da34-46fe-accc-d5dc3f866527"
                          "_756ede0c-89c8-49d5-9901-5487e7e635b1")
        expected-titles '("Adults (18-64 years)"
                          "From 65-84 years"
                          "85 years and over")]
    (is (= expected-ids (keys categories)))
    (is (= expected-titles (map :title (vals categories))))))

(deftest test-categories-rdf
  (let [categories   {"key" {:uri   [:qname :instance "category-uri"]
                             :title "category title"}}
        expected-rdf '([[:qname :instance "category-uri"]
                        ([[:qname :rdfs "label"] [:lit "category title"]]
                         [[:qname :rdf "type"] [:qname :ontology "Category"]])])]
    (is (= expected-rdf
           (categories-rdf-from-map categories)))))

(deftest test-find-baseline-groups
  (let [expected-groups '({:id          "baselineGroup1Id"
                           :armId       "arm1Id"
                           :sampleSize  "132"
                           :description "Subjects received semaglutide 0.25 mg subcutaneous (sc) injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly up to Week 30."}
                          {:id          "baselineGroup2Id"
                           :armId       "arm2Id"
                           :sampleSize  "131"
                           :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly for next 4 weeks and then semaglutide 1.0 mg once weekly up to week 30."}
                          {:id          "baselineGroup3Id"
                           :armId       "arm3Id"
                           :sampleSize  "133"
                           :description "Subjects received placebo (matched to semaglutide) sc injection once weekly for 30 weeks."})]
    (is (= expected-groups
           (find-baseline-groups xml)))))

(deftest test-find-arms
  (let [expected-arms '({:id          "arm1Id"
                         :title       "Semaglutide 0.5 mg"
                         :sampleSize  "132"
                         :description "Subjects received semaglutide 0.25 mg subcutaneous (sc) injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly up to Week 30."}
                        {:id          "arm2Id"
                         :title       "Semaglutide 1.0 mg"
                         :sampleSize  "131"
                         :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly for next 4 weeks and then semaglutide 1.0 mg once weekly up to week 30."}
                        {:id          "arm3Id"
                         :title       "Placebo"
                         :sampleSize  "133"
                         :description "Subjects received placebo (matched to semaglutide) sc injection once weekly for 30 weeks."})]
    (is (= expected-arms
           (find-arms xml)))))

(deftest test-find-adverse-event-groups
  (let [expected-groups '({:id          "ReportingGroup-1"
                           :title       "Semaglutide 0.5 mg"
                           :sampleSize  "132"
                           :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly up to Week 30."} 
                          {:id          "ReportingGroup-2"
                           :title       "Semaglutide 1.0 mg"
                           :sampleSize  "131"
                           :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly for next 4 weeks and then semaglutide 1.0 mg once weekly up to week 30."}
                          {:id          "ReportingGroup-3"
                           :title       "Placebo"
                           :sampleSize  "133"
                           :description "Subjects received placebo (matched to semaglutide) sc injection once weekly for 30 weeks."})]
       (is (= expected-groups
              (find-adverse-event-groups xml)))))

(deftest test-find-groups
  (is (every? #(= 3 %) 
      (map count (vals (find-groups xml))))))

(deftest test-group-uris
  (let [baseline-groups      (find-baseline-groups xml)
        adverse-event-groups (find-adverse-event-groups xml)
        arms                 (find-arms xml)
        group-uris           (build-group-uris arms 
                                               adverse-event-groups 
                                               baseline-groups)
        expected-ids         group-ids]
    (is (= expected-ids
           (keys group-uris)))
    (is (every? true?
                (map #(= (group-uris %1) (group-uris %2))
                     arm-ids baseline-group-ids)))))

(deftest test-group-rdf
  (let [baseline-groups      '({:id          "baselineGroup1Id"
                                :armId       "arm1Id"
                                :sampleSize  "132"
                                :description "baseline desc"}
                               {:id          "nonArmBaseline"
                                :armId       nil
                                :sampleSize  "131"
                                :description "non-arm baseline group"})
        adverse-event-groups '({:id          "ReportingGroup-1"
                                :title       "Semaglutide 0.5 mg"
                                :sampleSize  "132"
                                :description "adverse event group  desc"})
        arms                 '({:id          "arm1Id"
                                :title       "Semaglutide 0.5 mg"
                                :sampleSize  "132"
                                :description "arm group desc"})
        groups               {:arms                 arms
                              :baseline-groups      baseline-groups
                              :adverse-event-groups adverse-event-groups}
        mock-uri             (trig/iri :instance "generatedUuid")
        group-uris           (assoc (into {} (map
                                              #(vector % mock-uri)
                                              group-ids))
                                    "nonArmBaseline" mock-uri)
        found-groups-rdf     (groups-rdf groups group-uris)
        expected-groups-rdf  '([[:qname :instance "generatedUuid"]
                                ([[:qname :rdfs "label"] [:lit "Semaglutide 0.5 mg"]]
                                 [[:qname :rdfs "comment"] [:lit "arm group desc"]] 
                                 [[:qname :rdf "type"] [:qname :ontology "Arm"]])]
                               [[:qname :instance "generatedUuid"] 
                                ([[:qname :rdfs "label"] [:lit ""]] 
                                 [[:qname :rdfs "comment"] [:lit "non-arm baseline group"]]
                                 [[:qname :rdf "type"] [:qname :ontology "Group"]])]
                               [[:qname :instance "generatedUuid"] 
                                ([[:qname :rdfs "label"] [:lit ""]] 
                                 [[:qname :rdfs "comment"] [:lit "baseline desc"]] 
                                 [[:qname :rdf "type"] [:qname :ontology "Group"]])] 
                               [[:qname :instance "generatedUuid"]
                                ([[:qname :rdfs "label"] [:lit "Semaglutide 0.5 mg"]]
                                 [[:qname :rdfs "comment"] [:lit "adverse event group  desc"]]
                                 [[:qname :rdf "type"] [:qname :ontology "Group"]])])]
    (is (= expected-groups-rdf
           found-groups-rdf))))
