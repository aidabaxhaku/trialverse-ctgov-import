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
(def age-categorical (vtd/at xml "/result/baselineCharacteristics/ageCategoricalCharacteristic"))
(def age-continuous (vtd/at xml "/result/baselineCharacteristics/ageContinuousCharacteristic"))
(def insulin-dose-continuous (nth (vtd/search xml "/result/baselineCharacteristics/studyContinuousCharacteristics/studyContinuousCharacteristic")
                                  3))
(def decreased-appetite (first (find-adverse-events-xml xml)))

(def arm-ids '("arm1Id" "arm2Id" "arm3Id"))
(def baseline-group-ids '("baselineGroup1Id" "baselineGroup2Id" "baselineGroup3Id"))
(def adverse-event-group-ids '("ReportingGroup-1" "ReportingGroup-2" "ReportingGroup-3"))
(def group-ids (concat arm-ids adverse-event-group-ids baseline-group-ids))
(def variable-uri [:qname :instance "variable-uri"])
(def mm-uri [:qname :instance "mm-uri"])
(def age-category-ids '("adultsCategoryId" "pensionersCategoryId" "octogenarianCategoryId"))
(def mock-age-categories (into {} (map #(vector % {:uri [:qname :instance %]})
                                       age-category-ids)))
(def all-categories (find-categories xml))

(defn endpoints-one-through-x [x]
  (map #(vector :endpoint %) (range 1 (+ x 1))))

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
    (is (= [reg-uri (list [[:qname :ontology "registry"]
                           [:uri "http://trials.drugis.org/registries#EudraCT"]]
                          [[:qname :ontology "registration_id"]
                           [:lit eudract-id]]
                          [[:qname :bibo "uri"]
                           [:lit "https://www.clinicaltrialsregister.eu/ctr-search/trial/2013-004502-26/results"]])]
           built-registration))))

(deftest test-find-measurement-moments
  (let [[found-mm-ids found-mm-info] (find-measurement-moments xml)]
    (is (lib/same-ignoring-order?
          (concat (endpoints-one-through-x 8) '([:baseline] [:events]))
          (keys found-mm-ids)))
    (is (=
          '("Baseline"
             "From baseline to week 30"
             "From the first dose of trial product until the end of the post-treatment follow-up period.The follow-up visit was scheduled to take place 5 weeks after the date of last dose of trial product with a visit window of +7 days (maximum 36 weeks)."
             "After 30 weeks treatment"
             "After 30 weeks of treatment")
          (vals found-mm-info)))))

(deftest test-endpoint-xml-findable
  (is (= 8
         (count (vtd/search xml "/result/endPoints/endPoint")))))

(deftest test-variable-properties
  (let [found-properties (variable-properties hba1c-change-xml)
        expected-properties {:is-count?    false
                             :category-ids ()
                             :param        "MEASURE_TYPE.leastSquares"
                             :dispersion   "ENDPOINT_DISPERSION.standardError"
                             :units        "percentage of glycosylated hemoglobin"}]
    (is (= expected-properties found-properties))))

(deftest test-variable-properties-number
  (let [found-properties (variable-properties hba1c-under-7-percent-xml)
        expected-properties {:is-count?    false
                             :category-ids ()
                             :param        "MEASURE_TYPE.number"
                             :dispersion   "ENDPOINT_DISPERSION.na"
                             :units        "percentage of subjects"}]
    (is (= expected-properties found-properties))))

(deftest test-variable-properties-categorical
  (let [expected-properties {:is-count?    false
                             :category-ids age-category-ids
                             :param        nil
                             :dispersion   nil
                             :units        "Subjects"}
        found-properties (variable-properties age-categorical)]
    (is (= expected-properties found-properties))))

(deftest test-variable-results-properties-continuous
  (let [expected-results-properties-hba1c {:properties       '("least_squares_mean" "standard_error")
                                           :measurement-type "continuous"
                                           :dispersion       '("standard_error")
                                           :tendency         "least_squares_mean"
                                           :category-ids     '()}
        found-results-properties-hba1c (variable-results-properties hba1c-change-xml)
        expected-results-properties-insulin {:properties       '("median" "min" "max")
                                             :measurement-type "continuous"
                                             :dispersion       '("min" "max")
                                             :tendency         "median"
                                             :category-ids     '()}
        found-results-properties-insulin (variable-results-properties insulin-dose-continuous)]
    (is (= expected-results-properties-hba1c found-results-properties-hba1c))
    (is (= expected-results-properties-insulin found-results-properties-insulin))))

(deftest test-variable-results-properties-dichotomous
  (let [expected-results-properties {:properties       '("percentage")
                                     :measurement-type "dichotomous"
                                     :dispersion       '()
                                     :tendency         "percentage"
                                     :category-ids     '()}
        found-results-properties (variable-results-properties hba1c-under-7-percent-xml)]
    (is (= expected-results-properties found-results-properties))))

(deftest test-variable-results-properties-categorical
  (let [expected-results-properties {:properties       '()
                                     :measurement-type "categorical"
                                     :dispersion       '()
                                     :tendency         nil
                                     :category-ids     age-category-ids}
        found-results-properties (variable-results-properties age-categorical)]
    (is (= expected-results-properties found-results-properties))))

(deftest test-endpoint-rdf-least-squares
  (let [variable-uris {[:endpoint 1] [:qname :instance "variable-uri"]}
        mm-uris {[:endpoint 1] mm-uri}
        generated-rdf (endpoint-rdf hba1c-change-xml 1 variable-uris mm-uris)
        expected-properties '([[:qname :rdf "type"] [:qname :ontology "Endpoint"]]
                              [[:qname :rdfs "label"] [:lit "Change in HbA1c"]]
                              [[:qname :rdfs "comment"] [:lit "Estimated mean change from baseline in HbA1c at week 30. The post-baseline responses are analysed using a mixed model for repeated measurements with treatment, country and stratification variable (HbA1c level at screening [<= 8.0% or > 8.0%] crossed with use of metformin [yes or no]; 2 by 2 levels) as fixed factors and baseline value as covariate, all nested within visit. Mean estimates are adjusted according to observed baseline distribution. Missing data was imputed using mixed model for repeated measurements. Analysis was performed on full analysis set which included all randomised subjects who had received at least 1 dose of randomised semaglutide or placebo."]]
                              [[:qname :ontology "is_measured_at"]
                               [:qname :instance "mm-uri"]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "sample_size"]]
                              [[:qname :ontology "of_variable"]
                               [:blank ([[:qname :rdf "type"]
                                         [:qname :ontology "Variable"]]
                                        [[:qname :ontology "measurementType"]
                                         [:qname :ontology "continuous"]])]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "least_squares_mean"]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "standard_error"]])]
    (is (every? true? (map = expected-properties (second generated-rdf))))))

(deftest test-endpoint-rdf-number
  (let [variable-uris {[:endpoint 1] [:qname :instance "variable-uri"]}
        mm-uris {[:endpoint 1] [:qname :instance "mm-uri"]}
        generated-rdf (endpoint-rdf hba1c-under-7-percent-xml 1 variable-uris mm-uris)
        expected-properties '([[:qname :rdf "type"] [:qname :ontology "Endpoint"]]
                              [[:qname :rdfs "label"] [:lit "HbA1c below 7.0%"]]
                              [[:qname :rdfs "comment"] [:lit "Percentage of subjects with HbA1C below 7.0%"]]
                              [[:qname :ontology "is_measured_at"]
                               [:qname :instance "mm-uri"]]
                              [[:qname :ontology "has_result_property"] [:qname :ontology "sample_size"]]
                              [[:qname :ontology "of_variable"]
                               [:blank ([[:qname :rdf "type"]
                                         [:qname :ontology "Variable"]]
                                        [[:qname :ontology "measurementType"]
                                         [:qname :ontology "dichotomous"]])]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "percentage"]])]
    (is (every? true? (map = expected-properties (second generated-rdf))))))

(deftest test-endpoint-rdf-fullrange
  (let [variable-uris {[:endpoint 1] [:qname :instance "variable-uri"]}
        mm-uris {[:endpoint 1] [:qname :instance "mm-uri"]}
        generated-rdf (endpoint-rdf insulin-dose-continuous 1 variable-uris mm-uris)
        expected-properties '([[:qname :rdf "type"] [:qname :ontology "Endpoint"]]
                              [[:qname :rdfs "label"] [:lit "Insulin Dose"]]
                              [[:qname :rdfs "comment"] [:lit "Number of subjects analysed for this parameter=131, 131 and 133"]]
                              [[:qname :ontology "is_measured_at"]
                               [:qname :instance "mm-uri"]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "sample_size"]]
                              [[:qname :ontology "of_variable"]
                               [:blank ([[:qname :rdf "type"]
                                         [:qname :ontology "Variable"]]
                                        [[:qname :ontology "measurementType"]
                                         [:qname :ontology "continuous"]])]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "median"]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "min"]]
                              [[:qname :ontology "has_result_property"]
                               [:qname :ontology "max"]])]
    (is (every? true? (map = expected-properties (second generated-rdf))))))

(deftest test-find-adverse-events
  (is (= 45 (count (find-adverse-events-xml xml)))))

(deftest test-adverse-event-rdf-nonserious
  (let [variable-uris {[:event 1] [:qname :instance "variable-uri"]}
        mm-uris {[:events] [:qname :instance "mm-uri"]}
        expected-rdf-properties '([[:qname :rdf "type"] [:qname :ontology "AdverseEvent"]]
                                  [[:qname :rdfs "label"] [:lit "Decreased appetite"]]
                                  [[:qname :ontology "is_serious"] [:lit false]]
                                  [[:qname :rdfs "comment"] [:lit "Decreased appetite"]]
                                  [[:qname :ontology "is_measured_at"]
                                   [:qname :instance "mm-uri"]]
                                  [[:qname :ontology "of_variable"]
                                   [:blank ([[:qname :rdf "type"]
                                             [:qname :ontology "Variable"]]
                                            [[:qname :ontology "measurementType"]
                                             [:qname :ontology "dichotomous"]])]]
                                  [[:qname :ontology "has_result_property"]
                                   [:qname :ontology "sample_size"]]
                                  [[:qname :ontology "has_result_property"]
                                   [:qname :ontology "count"]]
                                  [[:qname :ontology "has_result_property"]
                                   [:qname :ontology "event_count"]])]
    (is (= expected-rdf-properties
           (second
             (adverse-event-rdf decreased-appetite
                                1
                                variable-uris
                                mm-uris))))))

(deftest test-find-baseline-xml
  (let [baseline-xml (find-baseline-xml xml)]
    (is (= 10 (count baseline-xml)))))

(deftest test-get-of-variable-rdf-non-categorical
  (let [measurement-type "continuous"
        result-properties {:category-ids age-category-ids}
        expected-rdf [[:qname :ontology "of_variable"]
                      [:blank (list
                                [[:qname :rdf "type"]
                                 [:qname :ontology "Variable"]]
                                [[:qname :ontology "measurementType"]
                                 [:qname :ontology "continuous"]])]]
        built-rdf (get-of-variable-rdf measurement-type result-properties mock-age-categories)]
    (is (= expected-rdf built-rdf))))

(deftest test-get-of-variable-rdf-categorical
  (let [measurement-type "categorical"
        result-properties {:category-ids age-category-ids}
        expected-rdf [[:qname :ontology "of_variable"]
                      [:blank (list
                                [[:qname :rdf "type"]
                                 [:qname :ontology "Variable"]]
                                [[:qname :ontology "measurementType"]
                                     [:qname :ontology "categorical"]]
                                    [[:qname :ontology "categoryList"]
                                     [:coll (list [:qname :instance "adultsCategoryId"]
                                                  [:qname :instance "pensionersCategoryId"]
                                                  [:qname :instance "octogenarianCategoryId"])]])]]
        built-rdf (get-of-variable-rdf measurement-type result-properties mock-age-categories)]
    (is (= expected-rdf built-rdf))))

(deftest test-baseline-var-rdf-continuous
  (let [uri [:qname :instance "baseline-uri"]
        baseline-uris {[:baseline 1] uri}
        mm-uris {[:baseline] [:qname :instance "mm-uri"]}
        expected-rdf-properties [uri '([[:qname :rdf "type"]
                                        [:qname :ontology "PopulationCharacteristic"]]
                                       [[:qname :rdfs "label"] [:lit "Age Continuous"]]
                                       [[:qname :ontology "is_measured_at"]
                                        [:qname :instance "mm-uri"]]
                                       [[:qname :ontology "of_variable"]
                                        [:blank ([[:qname :rdf "type"]
                                                  [:qname :ontology "Variable"]]
                                                 [[:qname :ontology "measurementType"]
                                                  [:qname :ontology "continuous"]])]]
                                       [[:qname :ontology "has_result_property"]
                                        [:qname :ontology "mean"]]
                                       [[:qname :ontology "has_result_property"]
                                        [:qname :ontology "standard_deviation"]])]]
    (is (= expected-rdf-properties
           (baseline-var-rdf age-continuous 1 baseline-uris mm-uris all-categories)))))

(deftest test-baseline-var-rdf-categorical
  (let [uri [:qname :instance "baseline-uri"]
        baseline-uris {[:baseline 1] uri}
        mm-uris {[:baseline] mm-uri}
        category-uris (map :uri (map all-categories age-category-ids))
        expected-rdf-properties [uri (list [[:qname :rdf "type"]
                                            [:qname :ontology "PopulationCharacteristic"]]
                                           [[:qname :rdfs "label"] [:lit "Age Categorical"]]
                                           [[:qname :ontology "is_measured_at"]
                                            mm-uri]
                                           [[:qname :ontology "of_variable"]
                                            [:blank (list
                                                      [[:qname :rdf "type"]
                                                       [:qname :ontology "Variable"]]
                                                      [[:qname :ontology "measurementType"]
                                                       [:qname :ontology "categorical"]]
                                                      [[:qname :ontology "categoryList"]
                                                       (trig/coll category-uris)])]])]
        found-baseline-rdf (baseline-var-rdf
                             age-categorical 1 baseline-uris mm-uris all-categories)]
    (is (= expected-rdf-properties
           found-baseline-rdf))))

(deftest test-find-categories
  (let [found-categories (find-categories xml)
        expected-ids '("femaleCategoryId"
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
    (is (= expected-ids (keys found-categories)))
    (is (= expected-titles (map :title (vals found-categories))))))

(deftest test-make-category-vector
  (let [uri [:qname :instance "uuid"]
        category-xml (first (vtd/search age-categorical "./categories/category"))
        category-info {:uri   uri
                       :title "Adults (18-64 years)"
                       :rdf   [uri
                               '([[:qname :rdfs "label"]
                                  [:lit "Adults (18-64 years)"]]
                                 [[:qname :rdf "type"]
                                  [:qname :ontology "Category"]])]}
        expected-result ["adultsCategoryId" category-info]]
    (is (= expected-result
           (make-category-vector category-xml uri)))))

(deftest test-find-baseline-groups
  (let [expected-groups '({:id          "baselineGroup1Id"
                           :arm-id      "arm1Id"
                           :sample-size 132
                           :description "Subjects received semaglutide 0.25 mg subcutaneous (sc) injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly up to Week 30."}
                          {:id          "baselineGroup2Id"
                           :arm-id      "arm2Id"
                           :sample-size 131
                           :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly for next 4 weeks and then semaglutide 1.0 mg once weekly up to week 30."}
                          {:id          "baselineGroup3Id"
                           :arm-id      "arm3Id"
                           :sample-size 133
                           :description "Subjects received placebo (matched to semaglutide) sc injection once weekly for 30 weeks."})]
    (is (= expected-groups
           (find-baseline-groups xml)))))

(deftest test-find-arms
  (let [expected-arms '({:id          "arm1Id"
                         :title       "Semaglutide 0.5 mg"
                         :sample-size 132
                         :description "Subjects received semaglutide 0.25 mg subcutaneous (sc) injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly up to Week 30."}
                        {:id          "arm2Id"
                         :title       "Semaglutide 1.0 mg"
                         :sample-size 131
                         :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly for next 4 weeks and then semaglutide 1.0 mg once weekly up to week 30."}
                        {:id          "arm3Id"
                         :title       "Placebo"
                         :sample-size 133
                         :description "Subjects received placebo (matched to semaglutide) sc injection once weekly for 30 weeks."})]
    (is (= expected-arms
           (find-arms xml)))))

(deftest test-find-adverse-event-groups
  (let [expected-groups '({:id          "ReportingGroup-1"
                           :title       "Semaglutide 0.5 mg"
                           :sample-size 132
                           :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly up to Week 30."}
                          {:id          "ReportingGroup-2"
                           :title       "Semaglutide 1.0 mg"
                           :sample-size 131
                           :description "Subjects received semaglutide 0.25 mg sc injection once weekly for 4 weeks followed by semaglutide 0.5 mg once weekly for next 4 weeks and then semaglutide 1.0 mg once weekly up to week 30."}
                          {:id          "ReportingGroup-3"
                           :title       "Placebo"
                           :sample-size 133
                           :description "Subjects received placebo (matched to semaglutide) sc injection once weekly for 30 weeks."})]
    (is (= expected-groups
           (find-adverse-event-groups xml)))))

(deftest test-find-groups
  (is (every? #(= 3 %)
              (map count (vals (find-groups xml))))))

(deftest test-build-group-uris
  (let [groups {:arms                 (find-arms xml)
                :adverse-event-groups (find-adverse-event-groups xml)
                :baseline-groups      (find-baseline-groups xml)}
        group-uris (build-group-uris groups)
        expected-ids group-ids]
    (is (= expected-ids
           (keys group-uris)))
    (is (every? true?
                (map #(= (group-uris %1) (group-uris %2))
                     arm-ids baseline-group-ids)))))

(deftest test-build-groups-with-uris
  (let [groups (find-groups xml)
        group-uris (build-group-uris groups)
        groups-with-uris (build-groups-with-uris groups group-uris)]
    (is (= 11 (count (apply concat (vals groups-with-uris)))))
    (is (every? true?
                (map (fn [group]
                       (= (:uri group)
                          (group-uris (:id group))))
                     (apply concat (vals groups-with-uris)))))))

(deftest test-build-groups-rdf
  (let [arm-1-uri [:qname :instance "arm1"]
        non-arm-baseline-uri [:qname :instance "nonArmBaseline"]
        adverse-event-1-uri [:qname :instance "adverseEvent1"]
        baseline-groups (list {:id          "baselineGroup1Id"
                               :title       "baseline arm"
                               :arm-id      "arm1Id"
                               :description "baseline descr"
                               :uri         arm-1-uri}
                              {:id          "nonArmBaseline"
                               :arm-id      nil
                               :title       "non arm baseline"
                               :description "non-arm baseline group"
                               :uri         non-arm-baseline-uri})
        adverse-event-groups (list {:id          "ReportingGroup-1"
                                    :title       "Semaglutide 0.5 mg"
                                    :description "adverse event group  desc"
                                    :uri         adverse-event-1-uri})
        arms (list {:id          "arm1Id"
                    :title       "Semaglutide 0.5 mg"
                    :description "arm group desc"
                    :uri         arm-1-uri})
        groups {:arms                 arms
                :baseline-groups      baseline-groups
                :adverse-event-groups adverse-event-groups
                :overall              {:title "Overall population"
                                       :uri   (lib/gen-uri)}}
        found-groups-rdf (build-groups-rdf groups)
        expected-groups-rdf '([[:qname :instance "arm1"]
                               ([[:qname :rdfs "label"] [:lit "Semaglutide 0.5 mg"]]
                                [[:qname :rdfs "comment"] [:lit "arm group desc"]]
                                [[:qname :rdf "type"] [:qname :ontology "Arm"]])]
                              [[:qname :instance "nonArmBaseline"]
                               ([[:qname :rdfs "label"] [:lit "non arm baseline"]]
                                [[:qname :rdfs "comment"] [:lit "non-arm baseline group"]]
                                [[:qname :rdf "type"] [:qname :ontology "Group"]])]
                              [[:qname :instance "adverseEvent1"]
                               ([[:qname :rdfs "label"] [:lit "Semaglutide 0.5 mg"]]
                                [[:qname :rdfs "comment"] [:lit "adverse event group  desc"]]
                                [[:qname :rdf "type"] [:qname :ontology "Group"]])])
        overall-group '([[:qname :rdfs "label"] [:lit "Overall population"]]
                        [[:qname :rdfs "comment"] [:lit ""]]
                        [[:qname :rdf "type"] [:qname :ontology "StudyPopulation"]])]
    (is (= 4 (count found-groups-rdf)))
    (is (= overall-group (-> found-groups-rdf
                             (nth 3)
                             (second))))
    (is (= expected-groups-rdf
           (take 3 found-groups-rdf)))))

(deftest test-build-all-groups-rdf
  (let [groups (find-groups xml)
        group-uris (build-group-uris groups)
        groups-with-uris (build-groups-with-uris groups group-uris)
        found-groups-rdf (build-groups-rdf groups-with-uris)]
    (is (= 7 (count found-groups-rdf)))))

(deftest test-read-endpoint-measurement
  (let [measurement-xml (first (vtd/search hba1c-change-xml "./armReportingGroups/armReportingGroup"))
        expected-result {:arm-id           "arm1Id"
                         :tendency-value   -1.45
                         :dispersion-value 0.09
                         :high-range-value nil
                         :sample-size      132}]
    (is (= expected-result
           (read-endpoint-measurement measurement-xml)))))

(deftest test-read-group-continuous-baseline-measurement-values
  (let [get-first-group (fn [baseline-characteristic]
                          (first (vtd/search baseline-characteristic
                                             "./reportingGroups/reportingGroup")))]
    (is (= {:arm-id           "baselineGroup1Id"
            :tendency-value   59.1
            :dispersion-value 10.3
            :high-range-value nil}
           (read-group-continuous-baseline-measurement-values
             (get-first-group age-continuous))))
    (is (= {:arm-id           "baselineGroup1Id"
            :tendency-value   35.0
            :dispersion-value 15.0
            :high-range-value 300.0}
           (read-group-continuous-baseline-measurement-values
             (get-first-group insulin-dose-continuous))))))

(deftest test-read-group-categorical-measurement-values
  (let [measurement-xml (first (vtd/search age-categorical "./reportingGroups/reportingGroup"))
        expected-result {"adultsCategoryId"       93
                         "pensionersCategoryId"   39
                         "octogenarianCategoryId" 0
                         :arm-id                  "baselineGroup1Id"}]
    (is (= expected-result
           (read-group-categorical-measurement-values measurement-xml "baselineReportingGroupId")))))

(deftest test-build-categorical-measurement-rdf
  (let
    [measurement {"category1" 3
                  "category2" 5
                  :arm-id     "groupId"}
     category-1-uri [:qname :instance "category1"]
     category-2-uri [:qname :instance "category2"]
     categories {"category1" {:uri category-1-uri}
                 "category2" {:uri category-2-uri}}
     group-uri [:qname :instance "groupId"]
     group-uris {"groupId" group-uri}
     expected-rdf (list [[:qname :ontology "of_outcome"] variable-uri]
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
                     variable-uri mm-uri group-uris categories))))))

(deftest test-build-continuous-measurement-rdf
  (let [group-1-uri [:qname :instance "baselineGroup1Id"]
        measurement {:arm-id           "baselineGroup1Id"
                     :tendency-value   35.0
                     :dispersion-value 15.0
                     :high-range-value 300.0}
        result-properties {:tendency   "mean"
                           :dispersion '("min" "max")}
        group-uris {"baselineGroup1Id" group-1-uri}
        sample-size 10
        expected-rdf (list [[:qname :ontology "of_outcome"]
                            [:qname :instance "variable-uri"]]
                           [[:qname :ontology "of_group"]
                            [:qname :instance "baselineGroup1Id"]]
                           [[:qname :ontology "of_moment"]
                            [:qname :instance "mm-uri"]]
                           [[:qname :ontology "sample_size"] [:lit sample-size]]
                           [[:qname :ontology "mean"] [:lit 35.0]]
                           [[:qname :ontology "min"] [:lit 15.0]]
                           [[:qname :ontology "max"] [:lit 300.0]])
        built-rdf (second (build-continuous-measurement-rdf
                            measurement result-properties
                            variable-uri mm-uri group-uris sample-size))]
    (is (= expected-rdf built-rdf))))

(deftest test-read-baseline-measurements-categorical
  (let [group1-uri [:qname :instance "baselineGroup1Id"]
        group2-uri [:qname :instance "baselineGroup2Id"]
        group3-uri [:qname :instance "baselineGroup3Id"]
        group-uris {"baselineGroup1Id" group1-uri
                    "baselineGroup2Id" group2-uri
                    "baselineGroup3Id" group3-uri}
        adults-category (:uri (all-categories "adultsCategoryId"))
        pensioners-category (:uri (all-categories "pensionersCategoryId"))
        octogenarian-category (:uri (all-categories "octogenarianCategoryId"))
        expected-rdf (list
                       (list [[:qname :ontology "of_outcome"] variable-uri]
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
                       (list [[:qname :ontology "of_outcome"] variable-uri]
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
                       (list [[:qname :ontology "of_outcome"] variable-uri]
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
                         variable-uri
                         mm-uri
                         group-uris
                         all-categories))))))

(deftest test-read-baseline-measurements-continuous-fullrange
  (let [group1-uri [:qname :instance "baselineGroup1Id"]
        group2-uri [:qname :instance "baselineGroup2Id"]
        group3-uri [:qname :instance "baselineGroup3Id"]
        group-uris {"baselineGroup1Id" group1-uri
                    "baselineGroup2Id" group2-uri
                    "baselineGroup3Id" group3-uri}
        sample-sizes {"baselineGroup1Id" 1
                      "baselineGroup2Id" 2
                      "baselineGroup3Id" 3}
        expected-rdf (list
                       (list [[:qname :ontology "of_outcome"] [:qname :instance "variable-uri"]]
                             [[:qname :ontology "of_group"] [:qname :instance "baselineGroup1Id"]]
                             [[:qname :ontology "of_moment"] [:qname :instance "mm-uri"]]
                             [[:qname :ontology "sample_size"] [:lit 1]]
                             [[:qname :ontology "median"] [:lit 35.0]]
                             [[:qname :ontology "min"] [:lit 15.0]]
                             [[:qname :ontology "max"] [:lit 300.0]])
                       (list [[:qname :ontology "of_outcome"] [:qname :instance "variable-uri"]]
                             [[:qname :ontology "of_group"] [:qname :instance "baselineGroup2Id"]]
                             [[:qname :ontology "of_moment"] [:qname :instance "mm-uri"]]
                             [[:qname :ontology "sample_size"] [:lit 2]]
                             [[:qname :ontology "median"] [:lit 36.0]]
                             [[:qname :ontology "min"] [:lit 14.0]]
                             [[:qname :ontology "max"] [:lit 320.0]])
                       (list [[:qname :ontology "of_outcome"] [:qname :instance "variable-uri"]]
                             [[:qname :ontology "of_group"] [:qname :instance "baselineGroup3Id"]]
                             [[:qname :ontology "of_moment"] [:qname :instance "mm-uri"]]
                             [[:qname :ontology "sample_size"] [:lit 3]]
                             [[:qname :ontology "median"] [:lit 36.0]]
                             [[:qname :ontology "min"] [:lit 12.0]]
                             [[:qname :ontology "max"] [:lit 124.0]]))
        found-rdf (map second (read-baseline-measurements-continuous
                                insulin-dose-continuous variable-uri mm-uri
                                group-uris sample-sizes))]
    (is (= expected-rdf found-rdf))))

(deftest test-read-endpoint-measurements-continuous
  (let [arm1-uri [:qname :instance "arm1Uri"]
        arm2-uri [:qname :instance "arm2Uri"]
        arm3-uri [:qname :instance "arm3Uri"]
        group-uris {"arm1Id" arm1-uri
                    "arm2Id" arm2-uri
                    "arm3Id" arm3-uri}
        expected-rdf (list
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] arm1-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 132]]
                             [[:qname :ontology "least_squares_mean"] [:lit -1.45]]
                             [[:qname :ontology "standard_error"] [:lit 0.09]])
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] arm2-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 131]]
                             [[:qname :ontology "least_squares_mean"] [:lit -1.85]]
                             [[:qname :ontology "standard_error"] [:lit 0.09]])
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] arm3-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 133]]
                             [[:qname :ontology "least_squares_mean"] [:lit -0.09]]
                             [[:qname :ontology "standard_error"] [:lit 0.09]]))]
    (is (= expected-rdf
           (map second (read-endpoint-measurements hba1c-change-xml
                                                   variable-uri
                                                   mm-uri
                                                   group-uris))))))

(deftest test-read-endpoint-measurements-dichotomous
  (let [arm1-uri [:qname :instance "arm1Uri"]
        arm2-uri [:qname :instance "arm2Uri"]
        arm3-uri [:qname :instance "arm3Uri"]
        group-uris {"arm1Id" arm1-uri
                    "arm2Id" arm2-uri
                    "arm3Id" arm3-uri}
        expected-rdf (list
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] arm1-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 132]]
                             [[:qname :ontology "percentage"] [:lit 60.6]])
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] arm2-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 131]]
                             [[:qname :ontology "percentage"] [:lit 78.6]])
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] arm3-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 133]]
                             [[:qname :ontology "percentage"] [:lit 10.5]]))]
    (is (= expected-rdf
           (map second (read-endpoint-measurements hba1c-under-7-percent-xml
                                                   variable-uri
                                                   mm-uri
                                                   group-uris))))))


(deftest test-read-adverse-event-measurements
  (let [group1-uri [:qname :instance "ReportingGroup-1"]
        group2-uri [:qname :instance "ReportingGroup-2"]
        group3-uri [:qname :instance "ReportingGroup-3"]
        group-uris {"ReportingGroup-1" group1-uri
                    "ReportingGroup-2" group2-uri
                    "ReportingGroup-3" group3-uri}
        expected-rdf (list
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] group1-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 132]]
                             [[:qname :ontology "count"] [:lit 5]]
                             [[:qname :ontology "event_count"] [:lit 5]])
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] group2-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 131]]
                             [[:qname :ontology "count"] [:lit 7]]
                             [[:qname :ontology "event_count"] [:lit 7]])
                       (list [[:qname :ontology "of_outcome"] variable-uri]
                             [[:qname :ontology "of_group"] group3-uri]
                             [[:qname :ontology "of_moment"] mm-uri]
                             [[:qname :ontology "sample_size"] [:lit 133]]
                             [[:qname :ontology "count"] [:lit 1]]
                             [[:qname :ontology "event_count"] [:lit 1]]))]
    (is (= expected-rdf
           (map second (read-adverse-event-measurements
                         decreased-appetite
                         variable-uri
                         mm-uri
                         group-uris))))))

(deftest test-find-sample-sizes
  (let [expected {"arm1Id" 132
                  "arm2Id" 131
                  "arm3Id" 133}
        found (find-sample-sizes (find-groups xml))]
    (is (= expected found))))

(deftest test-read-all-baseline-measurements
  (let [baseline-xml (find-baseline-xml xml)
        variable-uris (into {}
                            (map #(vector [:baseline %2]
                                          (lib/gen-uri))
                                 baseline-xml
                                 (iterate inc 1)))
        groups (find-groups xml)
        group-uris (build-group-uris groups)
        categories (find-categories xml)
        sample-sizes (find-sample-sizes groups)
        found (read-all-baseline-measurements baseline-xml mm-uri
                                              variable-uris group-uris categories
                                              sample-sizes)]
    (is (= (count found) (* 3 (count baseline-xml))))))

(deftest test-read-all-endpoint-measurements
  (let [endpoints-xml (find-endpoints-xml xml)
        n-endpoints (count endpoints-xml)
        variable-uris (into {}
                            (map #(vector [:endpoint %2]
                                          (lib/gen-uri))
                                 endpoints-xml
                                 (iterate inc 1)))
        mm-uris (zipmap (endpoints-one-through-x n-endpoints)
                        (repeat n-endpoints (lib/gen-uri)))
        groups (find-groups xml)
        group-uris (build-group-uris groups)
        categories (find-categories xml)
        found (read-all-endpoint-measurements endpoints-xml mm-uris
                                              variable-uris group-uris categories)
        has-nil-ontology? (fn [[pred _]]
                            (nil? (nth pred 2)))
        malformed-entries (filter (fn [pred-objs]
                                    (not (empty? (filter has-nil-ontology? pred-objs))))
                                  (map second found))]
    (is (= '() malformed-entries))
    (is (= (* (count (:arms groups)) (count endpoints-xml))
           (count found)))))

(deftest test-read-all-event-measurements
  (let [event-xml (find-adverse-events-xml xml)
        variable-uris (into {}
                            (map #(vector [:event %2] (lib/gen-uri))
                                 event-xml
                                 (iterate inc 1)))
        groups (find-groups xml)
        group-uris (build-group-uris groups)
        found (read-all-event-measurements event-xml mm-uri
                                           variable-uris group-uris)]

    (is (= (count found) (* 3 (count event-xml))))))

(deftest test-create-study-node
  (let [reg-uri [:qname :instance "reg-uri"]
        expected (list [[:qname :ontology "has_publication"] reg-uri]
                       [[:qname :rdf "type"] [:qname :ontology "Study"]]
                       [[:qname :rdfs "label"] [:lit "The full title"]]
                       [[:qname :rdfs "comment"] [:lit "The full title"]]
                       [[:qname :ontology "has_objective"]
                        [:blank '([[:qname :rdfs "comment"]
                                   [:lit "The primary objective"]])]]
                       [[:qname :ontology "has_eligibility_criteria"]
                        [:blank '([[:qname :rdfs "comment"]
                                   [:lit "Not applicable"]])]]
                       [[:qname :ontology "has_allocation"]
                        [:qname :ontology "AllocationRandomized"]]
                       [[:qname :ontology "has_blinding"]
                        [:qname :ontology "DoubleBlind"]])
        reg-uri [:qname :instance "reg-uri"]
        some-uris {:key1 variable-uri}
        groups (find-groups xml)
        group-uris (build-group-uris groups)
        groups-with-uris (build-groups-with-uris groups group-uris)
        found (create-study-node xml reg-uri some-uris some-uris
                                 some-uris groups-with-uris)]
    (is (= 18                                               ; 8 normal; 1 endpoint; 1 baseline var; 1 adverse event
           ; 3 arms; 3 groups; 1 overall population
           (count (second found))))
    (is (= expected (take 8 (second found))))))

(deftest test-import-eudract
  (let [found (import-eudract xml)]
    (spit "out.rdf" found)
    (is (= 111815 (count found)))))
; (defn p* [x] (clojure.pprint/pprint x) x)

; (deftest test-import-eudract2
;   (let [xml2  (vtd/navigator (slurp "test/app/eudract2.xml"))
;         found (import-eudract xml2)]

;     (spit "out.rdf" found)
;     (is (= 109737 (count found)))))

; ; ; (is (= 109737 (count found)))))
; ;         endpoints    (find-endpoints-xml xml2)
; ;         groups       (find-groups xml2)
; ;         group-uris   (build-group-uris groups)
; ;         categories   (find-categories xml2)
; ;         measurements (read-endpoint-measurements
; ;                       (nth endpoints 2)
; ;                       (lib/gen-uri)
; ;                       mm-uri group-uris)]
; ;     (is (= {} measurements))))
