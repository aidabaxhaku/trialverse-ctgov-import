(ns app.eudract-import-test
  (:require [riveted.core :as vtd]
            [app.import-shared :as lib]
            [clojure.set :refer [difference]]
            [org.drugis.addis.rdf.trig :as trig])
  (:use clojure.test)
  (:use app.eudract-import))

(def xml (vtd/navigator (slurp "test/app/eudract.xml")))
(def hba1c-change-xml (first (vtd/search xml "/result/endPoints/endPoint")))
(def hba1c-under-7-percent-xml (nth (vtd/search xml "/result/endPoints/endPoint")
                                    6))
(def bp-change-categorical (nth (vtd/search xml "/result/endPoints/endPoint")
                                4))
(def age-categorical (vtd/at xml "/result/baselineCharacteristics/ageCategoricalCharacteristic"))
(def age-continuous (vtd/at xml "/result/baselineCharacteristics/ageContinuousCharacteristic"))
(def decreased-appetite (first (find-adverse-events-xml xml)))

(def arm-ids '("arm1Id" "arm2Id" "arm3Id"))
(def baseline-group-ids '("baselineGroup1Id" "baselineGroup2Id" "baselineGroup3Id"))
(def adverse-event-group-ids '("ReportingGroup-1" "ReportingGroup-2" "ReportingGroup-3"))
(def group-ids (concat arm-ids adverse-event-group-ids baseline-group-ids))
(def age-categories (into {} (map #(vector % {:uri [:qname :instance %]})
                                  '("adultsCategoryId"
                                    "pensionersCategoryId"
                                    "octogenarianCategoryId"))))
(def outcome-uri  [:qname :instance "outcome-uri"])
(def mm-uri [:qname :instance "mm-uri"])
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

(deftest test-outcome-properties
  (let [found-properties (outcome-properties hba1c-change-xml)
        expected-properties {:simple     true
                             :is-count?  false
                             :categories ()
                             :param      "MEASURE_TYPE.leastSquares"
                             :dispersion "ENDPOINT_DISPERSION.standardError"
                             :units      "percentage of glycosylated hemoglobin"}]
    (is (= expected-properties found-properties))))

(deftest test-outcome-properties-number
  (let [found-properties (outcome-properties hba1c-under-7-percent-xml)
        expected-properties  {:simple     true
                              :is-count?  false
                              :categories ()
                              :param      "MEASURE_TYPE.number"
                              :dispersion "ENDPOINT_DISPERSION.na"
                              :units      "percentage of subjects"}]
    (is (= expected-properties found-properties))))

(deftest test-outcome-results-properties-continuous
  (is (= '("least_squares_mean" "standard_error")
         (:properties (outcome-results-properties hba1c-change-xml)))))

(deftest test-outcome-results-properties-dichotomous
  (is (= '("percentage")
         (:properties (outcome-results-properties hba1c-under-7-percent-xml)))))

(deftest test-outcome-rdf-least-squares
  (let [outcome-uris        {[:outcome 1] outcome-uri}
        mm-uris             {[:outcome 1] mm-uri}
        generated-rdf       (outcome-rdf hba1c-change-xml 1 outcome-uris mm-uris)
        expected-properties (list 
                             [[:qname :rdf "type"] [:qname :ontology "Endpoint"]]
                             [[:qname :rdfs "label"] [:lit "Change in HbA1c"]] 
                             [[:qname :rdfs "comment"] [:lit "Estimated mean change from baseline in HbA1c at week 30. The post-baseline responses are analysed using a mixed model for repeated measurements with treatment, country and stratification variable (HbA1c level at screening [<= 8.0% or > 8.0%] crossed with use of metformin [yes or no]; 2 by 2 levels) as fixed factors and baseline value as covariate, all nested within visit. Mean estimates are adjusted according to observed baseline distribution. Missing data was imputed using mixed model for repeated measurements. Analysis was performed on full analysis set which included all randomised subjects who had received at least 1 dose of randomised semaglutide or placebo."]] 
                             [[:qname :ontology "is_measured_at"] mm-uri] 
                             [[:qname :ontology "has_result_property"] [:qname :ontology "sample_size"]]
                             [[:qname :ontology "of_variable"]
                              [:blank (list
                                       [[:qname :ontology "measurementType"]
                                        [:qname :ontology "continuous"]])]]
                             [[:qname :ontology "has_result_property"] [:qname :ontology "least_squares_mean"]] 
                             [[:qname :ontology "has_result_property"] [:qname :ontology "standard_error"]])]
    (is (every? true? (map = expected-properties (second generated-rdf))))))

(deftest test-outcome-rdf-number
  (let [outcome-uris        {[:outcome 1] outcome-uri}
        mm-uris             {[:outcome 1] mm-uri}
        generated-rdf       (outcome-rdf hba1c-under-7-percent-xml 1 outcome-uris mm-uris)
        expected-properties (list
                             [[:qname :rdf "type"] [:qname :ontology "Endpoint"]]
                             [[:qname :rdfs "label"]  [:lit "HbA1c below 7.0%"]]
                             [[:qname :rdfs "comment"] [:lit "Percentage of subjects with HbA1C below 7.0%"]]
                             [[:qname :ontology "is_measured_at"] mm-uri]
                             [[:qname :ontology "has_result_property"] [:qname :ontology "sample_size"]]
                             [[:qname :ontology "of_variable"]
                              [:blank (list [[:qname :ontology "measurementType"]
                                        [:qname :ontology "dichotomous"]])]]
                             [[:qname :ontology "has_result_property"] [:qname :ontology "percentage"]])]
    (is (every? true? (map = expected-properties (second generated-rdf))))))

(deftest test-adverse-event-rdf-nonserious
  (let
   [outcome-uris            {[:event 1] outcome-uri}
    mm-uris                 {[:events] mm-uri}
    expected-rdf-properties (list 
                             [[:qname :rdf "type"] [:qname :ontology "AdverseEvent"]]
                             [[:qname :rdfs "label"] [:lit "Decreased appetite"]]
                             [[:qname :ontology "is_serious"] [:lit false]]
                             [[:qname :rdfs "comment"] [:lit "Decreased appetite"]]
                             [[:qname :ontology "is_measured_at"] mm-uri]
                             [[:qname :ontology "of_variable"]
                              [:blank (list 
                                       [[:qname :ontology "measurementType"]
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

(deftest test-find-adverse-events-xml
  (is (= 45 (count (find-adverse-events-xml xml)))))

(deftest test-find-baseline-xml
  (let [ baseline-xml (find-baseline-xml xml)]
    (is (= 10 (count baseline-xml)))))

(deftest test-find-endpoints-xml
  (is (= 8 (count (find-endpoints-xml xml)))))

(deftest test-find-variables-xml 
  (is (= 63 (count (find-variables-xml xml)))))

(deftest test-baseline-var-rdf-continuous
  (let [baseline-uris           {[:baseline 1] [:qname :instance "baseline-uri"]}
        mm-uris                 {[:baseline] mm-uri}
        expected-rdf-properties (list
                                 [[:qname :rdf "type"]
                                  [:qname :ontology "PopulationCharacteristic"]]
                                 [[:qname :rdfs "label"] [:lit "Age Continuous"]]
                                 [[:qname :ontology "is_measured_at"] mm-uri]
                                 [[:qname :ontology "of_variable"]
                                  [:blank (list
                                           [[:qname :ontology "measurementType"]
                                            [:qname :ontology "continuous"]])]]
                                 [[:qname :ontology "has_result_property"]
                                  [:qname :ontology "mean"]]
                                 [[:qname :ontology "has_result_property"]
                                  [:qname :ontology "standard_deviation"]])]
    (is (= expected-rdf-properties
           (second (baseline-var-rdf age-continuous 1 baseline-uris mm-uris))))))

(deftest test-baseline-var-rdf-categorical
  (let [baseline-uris           {[:baseline 1] [:qname :instance "baseline-uri"]}
        mm-uris                 {[:baseline] mm-uri}
        category-ids            '("adultsCategoryId"
                                  "pensionersCategoryId"
                                  "octogenarianCategoryId")
        category-uris           (map #(vector :qname :instance %) category-ids)
        categories              (map #(hash-map :uri %) category-uris)
        mock-result-properties  {:categories categories
                                 :properties []}
        expected-rdf-properties (list [[:qname :rdf "type"]
                                       [:qname :ontology "PopulationCharacteristic"]]
                                      [[:qname :rdfs "label"] [:lit "Age Categorical"]]
                                      [[:qname :ontology "is_measured_at"]
                                       mm-uri]
                                      [[:qname :ontology "of_variable"]
                                       [:blank (list
                                                [[:qname :ontology "measurementType"]
                                                 [:qname :ontology "categorical"]]
                                                [[:qname :ontology "categoryList"]
                                                 (trig/coll category-uris)])]])
        found-baseline-rdf       (with-redefs [outcome-results-properties
                                               (fn [x] mock-result-properties)]
                                   (second
                                    (baseline-var-rdf
                                     age-categorical 1 baseline-uris mm-uris)))]
      (is (= expected-rdf-properties
             found-baseline-rdf))))

(deftest test-find-categories
  (let [categories      (find-categories xml)
        expected-ids    '("femaleCategoryId"
                          "maleCategoryId"
                          "adultsCategoryId"
                          "pensionersCategoryId"
                          "octogenarianCategoryId"
                          "diastolicCategoryId"
                          "systolicCategoryId")
        expected-titles '("Female"
                          "Male"
                          "Adults (18-64 years)"
                          "From 65-84 years"
                          "85 years and over"
                          "Diastolic blood pressure"
                          "Systolic blood pressure")]
    (is (= expected-ids (keys categories)))
    (is (= expected-titles (map :title (vals categories))))))

(deftest test-make-category-vector
  (let [uri             [:qname :instance "uuid"]
        category-xml    (first (vtd/search age-categorical "./categories/category"))
        category-info   {:uri   uri
                         :title "Adults (18-64 years)"
                         :rdf   [uri
                                 '([[:qname :rdfs "label"] 
                                    [:lit "Adults (18-64 years)"]]
                                   [[:qname :rdf "type"] 
                                    [:qname :ontology "Category"]])]}
        expected-result ["adultsCategoryId" category-info]]
    (is (= expected-result
           (make-category-vector category-xml uri)))))

(deftest test-read-categorical-measurement-values-for-group
  (let [measurement-xml (first (vtd/search age-categorical "./reportingGroups/reportingGroup"))
        expected-result {"adultsCategoryId"       93
                         "pensionersCategoryId"   39
                         "octogenarianCategoryId" 0
                         :group-id "baselineGroup1Id"}]
    (is (= expected-result
           (read-categorical-measurement-values-for-group measurement-xml)))))

(deftest test-find-baseline-groups
  (let [expected-groups '({:id          "baselineGroup1Id"
                           :arm-id       "arm1Id"
                           :sampleSize  "132"
                           :description "Subjects received semaglutide 0.25 mg subcutaneous (sc) injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly up to Week 30."}
                          {:id          "baselineGroup2Id"
                           :arm-id       "arm2Id"
                           :sampleSize  "131"
                           :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly for next 4 weeks and then semaglutide 1.0 mg once weekly up to week 30."}
                          {:id          "baselineGroup3Id"
                           :arm-id       "arm3Id"
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

(deftest test-build-group-uris
  (let [baseline-groups      (find-baseline-groups xml)
        adverse-event-groups (find-adverse-event-groups xml)
        arms                 (find-arms xml)
        group-uris           (build-group-uris {:arms arms 
                                               :adverse-event-groups adverse-event-groups 
                                               :baseline-groups baseline-groups})
        expected-ids         group-ids]
    (is (= expected-ids
           (keys group-uris)))
    (is (every? true?
                (map #(= (group-uris %1) (group-uris %2))
                     arm-ids baseline-group-ids)))))

(deftest test-build-groups-rdf
  (let [baseline-groups      '({:id          "baselineGroup1Id"
                                :arm-id       "arm1Id"
                                :sampleSize  132
                                :description "baseline descr"}
                               {:id          "nonArmBaseline"
                                :arm-id       nil
                                :sampleSize  131
                                :description "non-arm baseline group"})
        adverse-event-groups '({:id          "ReportingGroup-1"
                                :title       "Semaglutide 0.5 mg"
                                :sampleSize  132
                                :description "adverse event group  desc"})
        arms                 '({:id          "arm1Id"
                                :title       "Semaglutide 0.5 mg"
                                :sampleSize  132
                                :description "arm group desc"})
        groups               {:arms                 arms
                              :baseline-groups      baseline-groups
                              :adverse-event-groups adverse-event-groups}
        mock-uris             (map #(trig/iri :instance (str "generatedUuid" %))
                                   (range 1 (+ 2 (count group-ids))))
        group-uris           (assoc (zipmap group-ids mock-uris)
                                    "nonArmBaseline" (last mock-uris))
        found-groups-rdf     (build-groups-rdf groups group-uris)
        expected-groups-rdf  '([[:qname :instance "generatedUuid1"]
                                ([[:qname :rdfs "label"] [:lit "Semaglutide 0.5 mg"]]
                                 [[:qname :rdfs "comment"] [:lit "arm group desc"]] 
                                 [[:qname :rdf "type"] [:qname :ontology "Arm"]])]
                               [[:qname :instance "generatedUuid7"] 
                                ([[:qname :rdfs "label"] [:lit ""]] 
                                 [[:qname :rdfs "comment"] [:lit "baseline descr"]] 
                                [[:qname :rdf "type"] [:qname :ontology "Group"]])] 
                               [[:qname :instance "generatedUuid10"]
                                ([[:qname :rdfs "label"] [:lit ""]]
                                 [[:qname :rdfs "comment"] [:lit "non-arm baseline group"]]
                                 [[:qname :rdf "type"] [:qname :ontology "Group"]])]
                               [[:qname :instance "generatedUuid4"]
                                ([[:qname :rdfs "label"] [:lit "Semaglutide 0.5 mg"]]
                                 [[:qname :rdfs "comment"] [:lit "adverse event group  desc"]]
                                 [[:qname :rdf "type"] [:qname :ontology "Group"]])])]
    (is (= expected-groups-rdf
           found-groups-rdf))))

(deftest test-read-endpoint-measurement
  (let [measurement-xml (first (vtd/search hba1c-change-xml "./armReportingGroups/armReportingGroup"))
        expected-result {:arm-id           "arm1Id"
                         :tendency-value   -1.45
                         :dispersion-value 0.09
                         :sample-size      132}]
    (is (= expected-result
           (read-endpoint-measurement measurement-xml)))))
          
(deftest test-read-endpoint-measurements-continuous
  (let [arm1-uri [:qname :instance "arm1Uri"]
        arm2-uri [:qname :instance "arm2Uri"]
        arm3-uri [:qname :instance "arm3Uri"]
        outcome-result-properties {:dispersion "standard_error"
                                   :tendency "least_squares_mean"}
        group-uris {"arm1Id" arm1-uri
                    "arm2Id" arm2-uri
                    "arm3Id" arm3-uri}
        expected-rdf (list
                      (list [[:qname :ontology "of_outcome"] outcome-uri]
                            [[:qname :ontology "of_group"] arm1-uri]
                            [[:qname :ontology "of_moment"] mm-uri]
                            [[:qname :ontology "sample_size"] [:lit 132]]
                            [[:qname :ontology "least_squares_mean"] [:lit -1.45]]
                            [[:qname :ontology "standard_error"] [:lit 0.09]])
                      (list [[:qname :ontology "of_outcome"] outcome-uri]
                            [[:qname :ontology "of_group"] arm2-uri]
                            [[:qname :ontology "of_moment"] mm-uri]
                            [[:qname :ontology "sample_size"] [:lit 131]]
                            [[:qname :ontology "least_squares_mean"] [:lit -1.85]]
                            [[:qname :ontology "standard_error"] [:lit 0.09]])
                      (list [[:qname :ontology "of_outcome"] outcome-uri]
                            [[:qname :ontology "of_group"] arm3-uri]
                            [[:qname :ontology "of_moment"] mm-uri]
                            [[:qname :ontology "sample_size"] [:lit 133]]
                            [[:qname :ontology "least_squares_mean"] [:lit -0.09]]
                            [[:qname :ontology "standard_error"] [:lit 0.09]]))]
    (is (= expected-rdf
           (map second (read-endpoint-measurements hba1c-change-xml
                                                   outcome-result-properties
                                                   outcome-uri
                                                   mm-uri
                                                   group-uris))))))

(deftest test-read-adverse-event-measurements
  (let [group1-uri   [:qname :instance "ReportingGroup-1"]
        group2-uri   [:qname :instance "ReportingGroup-2"]
        group3-uri   [:qname :instance "ReportingGroup-3"]
        group-uris   {"ReportingGroup-1" group1-uri
                      "ReportingGroup-2" group2-uri
                      "ReportingGroup-3" group3-uri}
        expected-rdf (list
                      (list [[:qname :ontology "of_outcome"] outcome-uri]
                            [[:qname :ontology "of_group"] group1-uri]
                            [[:qname :ontology "of_moment"] mm-uri]
                            [[:qname :ontology "sample_size"] [:lit 132]]
                            [[:qname :ontology "count"] [:lit 5]])
                      (list [[:qname :ontology "of_outcome"] outcome-uri]
                            [[:qname :ontology "of_group"] group2-uri]
                            [[:qname :ontology "of_moment"] mm-uri]
                            [[:qname :ontology "sample_size"] [:lit 131]]
                            [[:qname :ontology "count"] [:lit 7]])
                      (list [[:qname :ontology "of_outcome"] outcome-uri]
                            [[:qname :ontology "of_group"] group3-uri]
                            [[:qname :ontology "of_moment"] mm-uri]
                            [[:qname :ontology "sample_size"] [:lit 133]]
                            [[:qname :ontology "count"] [:lit 1]]))]
    (is (= expected-rdf
           (map second (read-adverse-event-measurements
                        decreased-appetite
                        outcome-uri
                        mm-uri
                        group-uris))))))

(deftest test-build-categorical-measurement-rdf
  (let
   [measurement    {"category1" 3
                    "category2" 5
                    :group-id "groupId"}
    category-1-uri [:qname :instance "category1"]
    category-2-uri [:qname :instance "category2"]
    categories     {"category1" {:uri category-1-uri}
                    "category2" {:uri category-2-uri}}
    group-uri      [:qname :instance "groupId"]
    group-uris     {"groupId" group-uri}
    expected-rdf   (list [[:qname :ontology "of_outcome"] outcome-uri]
                         [[:qname :ontology "of_group"] group-uri]
                         [[:qname :ontology "of_moment"] mm-uri]
                         [[:qname :ontology "category_count"]
                          [:blank (list
                                   [[:qname :ontology "category"] category-1-uri]
                                   [[:qname :ontology "count"] [:lit 3]])]]
                         [[:qname :ontology "category_count"]
                          [:blank (list
                                   [[:qname :ontology "category"] category-2-uri]
                                   [[:qname :ontology "count"] [:lit 5]])]])]
    (is (= expected-rdf
           (second (build-categorical-measurement-rdf
                    measurement
                    outcome-uri mm-uri group-uris categories))))))

(deftest test-read-baseline-measurements-categorical
  (let [group1-uri            [:qname :instance "baselineGroup1Id"]
        group2-uri            [:qname :instance "baselineGroup2Id"]
        group3-uri            [:qname :instance "baselineGroup3Id"]
        group-uris            {"baselineGroup1Id" group1-uri
                               "baselineGroup2Id" group2-uri
                               "baselineGroup3Id" group3-uri}
        adults-category       (:uri (age-categories "adultsCategoryId"))
        pensioners-category   (:uri (age-categories "pensionersCategoryId"))
        octogenarian-category (:uri (age-categories "octogenarianCategoryId"))
        expected-rdf          (list
                               (list [[:qname :ontology "of_outcome"] outcome-uri]
                                     [[:qname :ontology "of_group"] group1-uri]
                                     [[:qname :ontology "of_moment"] mm-uri]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] adults-category]
                                               [[:qname :ontology "count"] [:lit 93]])]]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] pensioners-category]
                                               [[:qname :ontology "count"] [:lit 39]])]]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] octogenarian-category]
                                               [[:qname :ontology "count"] [:lit 0]])]])
                               (list [[:qname :ontology "of_outcome"] outcome-uri]
                                     [[:qname :ontology "of_group"] group2-uri]
                                     [[:qname :ontology "of_moment"] mm-uri]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] adults-category]
                                               [[:qname :ontology "count"] [:lit 102]])]]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] pensioners-category]
                                               [[:qname :ontology "count"] [:lit 29]])]]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] octogenarian-category]
                                               [[:qname :ontology "count"] [:lit 0]])]])
                               (list [[:qname :ontology "of_outcome"] outcome-uri]
                                     [[:qname :ontology "of_group"] group3-uri]
                                     [[:qname :ontology "of_moment"] mm-uri]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] adults-category]
                                               [[:qname :ontology "count"] [:lit 86]])]]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] pensioners-category]
                                               [[:qname :ontology "count"] [:lit 46]])]]
                                     [[:qname :ontology "category_count"]
                                      [:blank (list
                                               [[:qname :ontology "category"] octogenarian-category]
                                               [[:qname :ontology "count"] [:lit 1]])]]))]
    (is (= expected-rdf
           (map second (read-baseline-measurements-categorical
                        age-categorical
                        outcome-uri
                        mm-uri
                        group-uris
                        age-categories))))))

(deftest test-read-all-measurements
  (let [[mm-uris mm-info]     (find-measurement-moments xml)
        groups                (find-groups xml)
        group-uris            (build-group-uris groups)
        groups-rdf            (build-groups-rdf groups group-uris)
        baseline-xml          (find-baseline-xml xml)
        baseline-var-rdf-data (map #(baseline-var-rdf %1 %2 group-uris mm-uris)
                                   baseline-xml
                                   (iterate inc 1))]
    (println group-uris)
    (println baseline-var-rdf-data)
    (is (= 1 1))))
