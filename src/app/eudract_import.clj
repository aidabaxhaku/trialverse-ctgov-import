(ns app.eudract-import
  (:require
   [app.import-shared :as lib]
   [riveted.core :as vtd]
   [clojure.string :refer [lower-case]]
   [clojure.set :refer [difference]]
   [app.design-parse :refer [parse-masking]]
   [org.drugis.addis.rdf.trig :as trig]))

(def CENTRAL-TENDENCY {"leastSquares" "least_squares_mean"
                       "arithmetic"   "mean"
                       "geometric"    "geometric_mean"
                       "log"          "log_mean"
                       "median"       "median"})

(def DISPERSION {"standardDeviation"             "standard_deviation"
                 "standardError"                 "standard_error"
                 "interQuartileRange"            '("first_quartile" "third_quartile")
                 "geometricCoefficientVariation" "geometric_coefficient_of_variation"
                 "fullRange"                     '("min" "max")})

(defn prefix-types
  [prefix types]
  (into {} (map
            (fn [[key value]] (vector (str prefix "." key) value))
            types)))

(def MEASURE-TYPES (prefix-types "MEASURE_TYPE" CENTRAL-TENDENCY))
(def ENDPOINT-DISPERSION-TYPES (prefix-types "ENDPOINT_DISPERSION" DISPERSION))
(def CENTRAL-TENDENCY-TYPES (prefix-types "CENTRAL_TENDENCY" CENTRAL-TENDENCY))
(def DISPERSION-TYPES (prefix-types "DISPERSION" DISPERSION))

(defn find-event-time-frame
  [xml]
  {[:events]
   (or
    (lib/text-at xml "/result/adverseEvents/timeFrame")
    "Unknown")})

(defn find-endpoint-time-frames
  [xml]
  (into {} (map #(vector [:outcome %2] (vtd/text %1))
                (vtd/search xml "/result/endPoints/endPoint/timeFrame")
                (iterate inc 1))))

(defn find-measurement-moments
  [xml]
  (lib/sort-equivalent-values (merge (find-event-time-frame xml)
                                     (find-endpoint-time-frames xml)
                                     {[:baseline] "Baseline"})
                              lower-case))

(defn get-eudract-number
  [xml]
  (lib/text-at xml "/result/@eudractNumber"))

(defn get-nct-id
  [xml]
  (lib/text-at xml "/result/trialInformation/usctnIdentifier"))

(defn build-registration 
    [reg-uri eudract-id]
    (trig/spo reg-uri 
              [(trig/iri :ontology "registry")
               (trig/iri "http://trials.drugis.org/registries#EudraCT")]        
              [(trig/iri :ontology "registration_id") 
               (trig/lit eudract-id)]
              [(trig/iri :bibo "uri") 
               (str "https://www.clinicaltrialsregister.eu/ctr-search/trial/" 
                    eudract-id 
                    "/results")]))

(defn outcome-properties
  [xml]
  (let [categories-xml (vtd/search xml "./categories/category")
        category-count (count (vtd/children categories-xml))
        category-info  (map (fn [category-xml]
                              {:id   (vtd/attr category-xml "id")
                               :name (lib/text-at category-xml "name")
                               :uri  (lib/gen-uri)})
                            categories-xml)
        param          (lib/text-at xml "centralTendencyType/value")
        dispersion     (lib/text-at xml "dispersionType/value")
        units          (lib/text-at xml "unit")]
    {:simple     (< category-count 2)
     :is-count?  (= "true" (lib/text-at xml "countable"))
     :categories category-info
     :param      param
     :dispersion dispersion
     :units      units}))

(defn is-percentage?
  [props]
  (and (= "MEASURE_TYPE.number" (:param props))
       (lib/string-starts-with-any?
        (lower-case (:units props)) 
        ["percent" "percentage" "percentages" "%" "observed percentage"])))

(defn is-proportion?
  [props]
  (and (= "MEASURE_TYPE.number" (:param props))
       (lib/string-starts-with-any? (lower-case (:units props)) ["proportion"])))

(defn outcome-measurement-type
  [props]
  (if (= "MEASURE_TYPE.number" (:param props)) "dichotomous" "continuous"))

; determine results properties from the measurement properties
; FIXME: CIs/quantiles
(defn outcome-results-properties
  [xml]
  (let [props            (outcome-properties xml)
        found-tendency   (filter
                          (comp not nil?)
                          (list
                           (MEASURE-TYPES (:param props))
                           (CENTRAL-TENDENCY-TYPES (:param props))
                           (if (:is-count? props) "count")
                           (if (is-percentage? props) "percentage")
                           (if (is-proportion? props) "proportion")))
        found-dispersion (filter
                          (comp not nil?)
                          (list
                          (ENDPOINT-DISPERSION-TYPES (:dispersion props))
                          (DISPERSION-TYPES (:dispersion props))))]
    {:properties (concat found-tendency found-dispersion)
     :measurement-type (outcome-measurement-type props)
     :dispersion found-dispersion
     :tendency   found-tendency}))

  (defn outcome-rdf
    [xml idx outcome-uris mm-uris]
    (let [uri        (outcome-uris [:outcome idx])          
          properties (outcome-results-properties xml)]
      (lib/spo-each
       (trig/spo uri
                 [(trig/iri :rdf "type") 
                  (trig/iri :ontology "Endpoint")]
                 [(trig/iri :rdfs "label") 
                  (trig/lit (lib/text-at xml "./title"))]
                 [(trig/iri :rdfs "comment") 
                  (trig/lit (or (lib/text-at xml "./description") ""))]
                 [(trig/iri :ontology "is_measured_at") 
                  (mm-uris [:outcome idx])]
                 [(trig/iri :ontology "has_result_property") 
                  (trig/iri :ontology "sample_size")]
                 [(trig/iri :ontology "of_variable")
                  (trig/_po [(trig/iri :ontology "measurementType") 
                             (trig/iri :ontology (:measurement-type properties))])])
       (trig/iri :ontology "has_result_property")
       (map #(trig/iri :ontology %) (:properties properties)))))

(defn find-adverse-events
  [xml] 
  (concat (vtd/search xml "/result/adverseEvents/nonSeriousAdverseEvents/nonSeriousAdverseEvent")
          (vtd/search xml "/result/adverseEvents/seriousAdverseEvents/seriousAdverseEvent")))


(defn adverse-event-rdf
  [xml idx event-uris mm-uris]
  (let [uri        (event-uris [:event idx])
        event-name (lib/text-at xml "./term")]
    (lib/spo-each
     (trig/spo
      uri
      [(trig/iri :rdf "type")
       (trig/iri :ontology "AdverseEvent")]
      [(trig/iri :rdfs "label")
       (trig/lit event-name)]
      [(trig/iri :ontology "is_serious")
       (trig/lit (= "seriousAdverseAvent" (vtd/tag xml)))]
      [(trig/iri :rdfs "comment")
       (trig/lit event-name)]
      [(trig/iri :ontology "is_measured_at")
       (mm-uris [:events])]
      [(trig/iri :ontology "of_variable")
       (trig/_po [(trig/iri :ontology "measurementType")
                  (trig/iri :ontology "dichotomous")])])
     (trig/iri :ontology "has_result_property")
     (map #(trig/iri :ontology %) ["sample_size" "count" "event_count"]))))

(defn find-baseline-xml
  [xml]
  (let [base "/result/baselineCharacteristics/"]
    {:continuous  (concat
                   (vtd/search xml (str base "studyContinuousCharacteristics/studyContinuousCharacteristic"))
                   (vtd/search xml (str base "ageContinuousCharacteristic")))
     :categorical (concat
                   (vtd/search xml (str base "studyCategoricalCharacteristics/studyCategoricalCharacteristic"))
                   (vtd/search xml (str base "genderCategoricalCharacteristic"))
                   (vtd/search xml (str base "ageCategoricalCharacteristic")))}))

(defn measurement-type-for-baseline
  [tag]
  (case tag
    ("studyCategoricalCharacteristic"
     "genderCategoricalCharacteristic"
     "ageCategoricalCharacteristic")  "categorical"
    ("studyContinuousCharacteristic"
     "ageContinuousCharacteristic") "continuous"))

(defn p* [x] (println x) x) ; FIXME: debug

(defn baseline-var-rdf
  [xml idx baseline-uris mm-uris]
  (let [uri                 (baseline-uris [:baseline idx])
        mm-uri              (mm-uris [:baseline])
        characteristic-name (lib/text-at xml "./title")
        measurement-type    (measurement-type-for-baseline (vtd/tag xml))
        result-properties   (outcome-results-properties xml)]
    (lib/spo-each
     (trig/spo uri
               [(trig/iri :rdf "type")
                (trig/iri :ontology "PopulationCharacteristic")]
               [(trig/iri :rdfs "label")
                (trig/lit characteristic-name)]
               [(trig/iri :ontology "is_measured_at") mm-uri]
               [(trig/iri :ontology "of_variable")
                (trig/_po [(trig/iri :ontology "measurementType")
                           (trig/iri :ontology measurement-type)])])
     (trig/iri :ontology "has_result_property")
     (map #(trig/iri :ontology %) (:properties result-properties)))))

(defn find-baseline-groups
  [xml]
  (let [groups-xml (vtd/search xml "/result/baselineCharacteristics/baselineReportingGroups/baselineReportingGroup")]
    (map (fn [%] {:id          (vtd/attr % "id")
                  :arm-id       (vtd/attr % "armId")
                  :sampleSize  (lib/text-at % "subjects")
                  :description (lib/text-at % "description")})
         groups-xml)))

(defn find-arms
  [xml]
  (let [arms-xml (vtd/search xml "/result/subjectDisposition/postAssignmentPeriods/postAssignmentPeriod/arms/arm")]
    (map (fn [%] {:id          (vtd/attr % "id")
                  :title       (lib/text-at % "title")
                  :sampleSize  (lib/text-at % "startedMilestoneAchievement/subjects")
                  :description (lib/text-at % "description")})
         arms-xml)))

(defn find-adverse-event-groups
  [xml]
  (let [groups-xml (vtd/search xml "/result/adverseEvents/reportingGroups/reportingGroup")]
    (map (fn [%] {:id          (vtd/attr % "id")
                  :title       (lib/text-at % "title")
                  :sampleSize  (lib/text-at % "subjectsExposed")
                  :description (lib/text-at % "description")})
         groups-xml)))

(defn build-group-uris
  [arms adverse-event-groups baseline-groups]
  (let [uris-by-id
        (into {}
              (map #(vector (:id %) (lib/gen-uri))
                   (concat arms adverse-event-groups)))
        baseline-uris-by-id
        (into {}
              (map #(vector (:id %) (uris-by-id (:arm-id %)))
                   baseline-groups))]
    (merge uris-by-id baseline-uris-by-id)))

(defn find-groups
 ; FIXME: consider <totalBaselineGroup> from categorical baselines? 
  [xml]
  {:arms                 (find-arms xml)
   :baseline-groups      (find-baseline-groups xml)
   :adverse-event-groups (find-adverse-event-groups xml)})

(defn groups-rdf
  [{arms                 :arms
    baseline-groups      :baseline-groups
    adverse-event-groups :adverse-event-groups}
   group-uris]
  (let [non-arm-baseline-groups (difference (set baseline-groups)
                                            (set arms))]
    (concat
     (map #(lib/arm-rdf (group-uris (:id %)) %)
          arms)
     (map #(lib/group-rdf (group-uris (:id %)) %)
          (concat non-arm-baseline-groups adverse-event-groups)))))

(defn read-endpoint-measurement
  [xml]
  {:arm-id           (vtd/attr xml "armId")
   :tendency-value   (lib/parse-double (lib/text-at xml "tendencyValues/tendencyValue/value"))
   :dispersion-value (lib/parse-double (lib/text-at xml "dispersionValues/dispersionValue/value"))
   :sample-size      (lib/parse-int (lib/text-at xml "subjects"))})

(defn build-continuous-measurement-rdf
  [measurement result-properties outcome-uri mm-uri group-uris]
  (as-> measurement m
    (lib/measurement-meta-rdf  (lib/gen-uri)
                               outcome-uri
                               (group-uris (:arm-id m))
                               mm-uri)
    (trig/spo m [(trig/iri :ontology "sample_size")
                 (trig/lit (:sample-size measurement))])
       ; eudract does not currently sypport >1 central tendency and dispersion properties
    (trig/spo m
              [(trig/iri :ontology (:tendency result-properties))
               (trig/lit (:tendency-value measurement))]
              [(trig/iri :ontology (:dispersion result-properties))
               (trig/lit (:dispersion-value measurement))])))

(defn read-endpoint-measurements
  [xml outcome-results-properties outcome-uri mm-uri group-uris]
  (let [measurements-xml (vtd/search xml "./armReportingGroups/armReportingGroup")
        measurements     (map read-endpoint-measurement measurements-xml)]
    (map #(build-continuous-measurement-rdf
           % outcome-results-properties
           outcome-uri mm-uri group-uris)
         measurements)))

(defn read-adverse-event-measurement
  [xml]
  {:group-id    (vtd/attr  xml "reportingGroupId")
   :count       (lib/parse-int (lib/text-at xml "./occurrences"))
   :sample-size (lib/parse-int (lib/text-at xml "subjectsExposed"))})

(defn build-dichotomous-measurement-rdf
  [measurement outcome-uri mm-uri group-uris]
  (as-> measurement meas
    (lib/measurement-meta-rdf (lib/gen-uri)
                              outcome-uri
                              (group-uris (:group-id meas))
                              mm-uri)
    (trig/spo meas [(trig/iri :ontology "sample_size")
                    (trig/lit (:sample-size measurement))]
              [(trig/iri :ontology "count")
               (trig/lit (:count measurement))])))

(defn read-adverse-event-measurements
  [xml outcome-uri mm-uri group-uris]
  (let [measurements-xml (vtd/search xml "./values/value")
        measurements     (map read-adverse-event-measurement measurements-xml)]
    (map #(build-dichotomous-measurement-rdf % outcome-uri mm-uri group-uris)
         measurements)))

(defn make-category-vector
  [category-xml]
  (let [uri   (lib/gen-uri)
        title (lib/text-at category-xml "name")]
    [(vtd/attr category-xml "id")
     {:uri   uri
      :title title
      :rdf   (trig/spo uri
                       [(trig/iri :rdfs "label")
                        (trig/lit title)]
                       [(trig/iri :rdf "type")
                        (trig/iri :ontology "Category")])}]))

(defn get-categories-for-variable
  [xml]
  (let [categories-xml (vtd/search xml "./categories/category")]
    (into {}
          (map #(make-category-vector %)
               categories-xml))))

(defn read-categorical-measurement[] {})

(defn build-categorical-measurement-rdf[]{})

(defn read-baseline-measurements-categorical
  [xml outcome-uri mm-uri group-uris]
  (let [categories       (get-categories-for-variable xml)
        measurements-xml (vtd/search xml "/reportingGroups/reportingGroup")
        measurements     (map #(read-categorical-measurement % categories)
                              measurements-xml)]
    (map #(build-categorical-measurement-rdf % outcome-uri mm-uri group-uris
                                             categories))))
; (defn )          
; (defn import-xml
;   [xml]
;   (let [
;         eudract-id (get-eudract-number xml)
;         nct-id (get-nct-id xml)
;         uri (trig/iri :study eudract-id)
;         reg-uri (trig/iri :ictrp eudract-id)
;         registration (build-registration reg-uri eudract-id)
;         [mm-uris mm-info] (find-measurement-moments xml)
;         outcome-xml (vtd/search xml "/clinical_study/clinical_result/outcome_list/outcome")
;         outcome-uris (build-outome-uris outcome-xml)
;         outcomes-rdf (map #(outcome-rdf %1 %2 outcome-uris mm-uris) 
;                           outcome-xml 
;                           (iterate inc 1))
;         event-xml (vtd/search xml "/clinical_study/clinical_results/reported_events/*//category_list/category/event_list/event")
        ; event-uris (build-event-uris)
;         events-rdf (map #(adverse-event-rdf %1 %2 event-uris mm-uris) event-xml (iterate inc 1))
;         baseline-xml (vtd/search xml "/clinical_study/clinical_results/baseline/measure_list/measure")
;         baseline-sample-size-xml (vtd/at xml "/clinical_study/clinical_results/baseline/analyzed_list/analyzed")
;         baseline-var-xml (rest baseline-xml)
;         baseline-uris (into {} (map #(vector %2 (trig/iri :instance (lib/uuid))) baseline-var-xml (iterate inc 1)))
;         baseline-data (map #(baseline-var-rdf %1 %2 baseline-uris mm-uris) baseline-var-xml (iterate inc 1))
;         baseline-rdf (map second baseline-data)
;         baseline-categories-data (map first baseline-data)
;         category-uris (reduce #(merge %1 (:uris %2)) {} baseline-categories-data)
;         category-rdf (reduce #(concat %1 (:rdfs %2)) [] baseline-categories-data)
;         [group-uris group-info] (find-groups xml)
;         groups-rdf (map #(group-rdf (first %) (second %)) group-info)
;         mms-rdf (map #(mm-rdf (first %) (second %)) mm-info)
        ; measurements-rdf (concat
        ;                   (apply concat
        ;                          (map #(baseline-measurements
        ;                                 %1 %2
        ;                                 baseline-sample-size-xml
        ;                                 baseline-uris group-uris
        ;                                 mm-uris category-uris)
        ;                               baseline-var-xml (iterate inc 1)))
        ;                   (apply concat
        ;                          (map #(outcome-measurements
        ;                                 %1 %2 outcome-uris group-uris
        ;                                 mm-uris)
        ;                               outcome-xml (iterate inc 1)))
        ;                   (apply concat
        ;                          (map #(event-measurements
        ;                                 %1 %2 event-uris group-uris mm-uris)
        ;                               event-xml (iterate inc 1))))
;         study-rdf (-> uri
;                      (trig/spo [(trig/iri :ontology "has_publication") reg-uri]
;                                [(trig/iri :rdf "type") (trig/iri :ontology "Study")]
;                                [(trig/iri :rdfs "label") (trig/lit (lib/text-at xml "/clinical_study/brief_title")))]
;                                [(trig/iri :rdfs "comment") (trig/lit (lib/text-at xml "/clinical_study/official_title")))]
;                                [(trig/iri :ontology "has_objective")
;                                 (trig/_po [(trig/iri :rdfs "comment") (trig/lit (lib/text-at xml "/clinical_study/brief_summary/textblock")))])]
;                                [(trig/iri :ontology "has_eligibility_criteria")
;                                 (trig/_po [(trig/iri :rdfs "comment") (trig/lit (lib/text-at xml "/clinical_study/eligibility/criteria/textblock")))])])

;                      (allocation-rdf (lib/text-at xml "/clinical_study/study_design_info/allocation")))
;                      (blinding-rdf (parse-masking (lib/text-at xml "/clinical_study/study_design_info/masking"))))
;                      (lib/spo-each (trig/iri :ontology "has_outcome") (vals baseline-uris))
;                      (lib/spo-each (trig/iri :ontology "has_outcome") (vals outcome-uris))
;                      (lib/spo-each (trig/iri :ontology "has_outcome") (vals event-uris))
;                      (lib/spo-each (trig/iri :ontology "has_group") (keys group-info)))
;         triples (concat [study-rdf registration] mms-rdf baseline-rdf category-rdf outcomes-rdf events-rdf groups-rdf measurements-rdf)]
;     (trig/write-ttl lib/prefixes triples)))
