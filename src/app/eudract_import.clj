(ns app.eudract-import
  (:require
   [app.import-shared :as lib]
   [riveted.core :as vtd]
   [clojure.string :refer [lower-case]]
   [clojure.set :refer [difference]]
   [org.drugis.addis.rdf.trig :as trig]))

(def ALLOCATION {"ALLOCATION.randControlled"    "AllocationRandomized"
                 "ALLOCATION.nonRandControlled" "AllocationNonRandomized"
                 "ALLOCATION.na"                "Unknown"})

(def BLINDING_TYPE {"BLINDING.single" "SingleBlind"
                    "BLINDING.double" "DoubleBlind"
                    "BLINDING.not" "OpenLabel"})

(def CENTRAL-TENDENCY {"leastSquares" "least_squares_mean"
                       "arithmetic"   "mean"
                       "geometric"    "geometric_mean"
                       "log"          "log_mean"
                       "median"       "median"})

(def DISPERSION {"standardDeviation"             '("standard_deviation")
                 "standardError"                 '("standard_error")
                 "interQuartileRange"            '("first_quartile" "third_quartile")
                 "geometricCoefficientVariation" '("geometric_coefficient_of_variation")
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
  (into {} (map #(vector [:endpoint %2] (vtd/text %1))
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

(defn find-adverse-events-xml
  [xml]
  (concat (vtd/search xml "/result/adverseEvents/nonSeriousAdverseEvents/nonSeriousAdverseEvent")
          (vtd/search xml "/result/adverseEvents/seriousAdverseEvents/seriousAdverseEvent")))

(defn find-baseline-xml
  [xml]
  (let [base "/result/baselineCharacteristics/"]
    (concat
     (vtd/search xml (str base "studyContinuousCharacteristics/studyContinuousCharacteristic"))
     (vtd/search xml (str base "ageContinuousCharacteristic"))
     (vtd/search xml (str base "studyCategoricalCharacteristics/studyCategoricalCharacteristic"))
     (vtd/search xml (str base "genderCategoricalCharacteristic"))
     (vtd/search xml (str base "ageCategoricalCharacteristic")))))

(defn find-endpoints-xml
  [xml]
  (vtd/search xml "/result/endPoints/endPoint"))

(defn find-variables-xml
  [xml]
  (concat (find-baseline-xml xml)
          (find-adverse-events-xml xml)
          (find-endpoints-xml xml)))

(defn variable-properties
  [xml]
  (let [categories-xml (vtd/search xml "./categories/category")
        category-ids   (map #(vtd/attr % "id") categories-xml)
        param          (lib/text-at xml "centralTendencyType/value")
        dispersion     (lib/text-at xml "dispersionType/value")
        units          (lib/text-at xml "unit")]
    {:is-count?    (= "true" (lib/text-at xml "countable"))
     :category-ids category-ids
     :param        param
     :dispersion   dispersion
     :units        units}))

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

(defn is-confidence-interval?
  [props]
  (= "ENDPOINT_DISPERSION.confidenceInterval" (:dispersion props)))

(defn measurement-type
  [props]
  (if (= "MEASURE_TYPE.number" (:param props)) "dichotomous" "continuous"))

(defn get-quantiles
  [xml]
  (case (lib/parse-int (lib/text-at xml "./percentage"))
    90 '("quantile_0.05" "quantile_0.95")
    95 '("quantile_0.025" "quantile_0.975")
    default (throw "unsupported confidence interval width")))

; determine results properties from the measurement properties
(defn variable-results-properties
  [xml]
  (let [props            (variable-properties xml)
        category-ids     (:category-ids props)
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
                          (concat
                           (ENDPOINT-DISPERSION-TYPES (:dispersion props))
                           (DISPERSION-TYPES (:dispersion props))
                           (if (is-confidence-interval? props) (get-quantiles xml))))]
    {:properties       (concat found-tendency found-dispersion)
     :measurement-type (if (or (.endsWith (vtd/tag xml) "CategoricalCharacteristic")
                               (and (:is-count? props)
                                    (> (count category-ids) 1)))
                         "categorical"
                         (measurement-type props))
     :dispersion       found-dispersion
     :tendency         (first found-tendency)
     :category-ids     category-ids}))

  (defn endpoint-rdf
    [xml idx variable-uris mm-uris]
    (let [uri        (variable-uris [:endpoint idx])          
          properties (variable-results-properties xml)]
      (lib/spo-each
       (trig/spo uri
                 [(trig/iri :rdf "type") 
                  (trig/iri :ontology "Endpoint")]
                 [(trig/iri :rdfs "label") 
                  (trig/lit (lib/text-at xml "./title"))]
                 [(trig/iri :rdfs "comment") 
                  (trig/lit (or (lib/text-at xml "./description") ""))]
                 [(trig/iri :ontology "is_measured_at") 
                  (mm-uris [:endpoint idx])]
                 [(trig/iri :ontology "has_result_property") 
                  (trig/iri :ontology "sample_size")]
                 [(trig/iri :ontology "of_variable")
                  (trig/_po [(trig/iri :ontology "measurementType") 
                             (trig/iri :ontology (:measurement-type properties))])])
       (trig/iri :ontology "has_result_property")
       (map #(trig/iri :ontology %) (:properties properties)))))


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


(defn measurement-type-for-baseline
  [tag]
  (case tag
    ("studyCategoricalCharacteristic"
     "genderCategoricalCharacteristic"
     "ageCategoricalCharacteristic")  "categorical"
    ("studyContinuousCharacteristic"
     "ageContinuousCharacteristic") "continuous"))

; (defn p* [x] (clojure.pprint/pprint x) x) ; FIXME: debug

(defn get-of-variable-rdf
  [measurement-type result-properties categories]
  (let
   [category-uris (map :uri (map categories (:category-ids result-properties)))
    category-rdf  (if (= measurement-type "categorical")
                    [(trig/iri :ontology "categoryList")
                     (trig/coll category-uris)])
    variable-base [(trig/iri :ontology "measurementType")
                   (trig/iri :ontology measurement-type)]
    of-variable   (if category-rdf
                    (trig/_po variable-base category-rdf)
                    (trig/_po variable-base))]
    [(trig/iri :ontology "of_variable")
     of-variable]))

(defn baseline-var-rdf
  [xml idx baseline-uris mm-uris categories]
  (let
   [uri               (baseline-uris [:baseline idx])
    mm-uri            (mm-uris [:baseline])
    name              (lib/text-at xml "./title")
    measurement-type  (measurement-type-for-baseline (vtd/tag xml))
    result-properties (variable-results-properties xml)
    variable-rdf      (get-of-variable-rdf measurement-type result-properties categories)]
    (lib/spo-each
     (trig/spo uri
               [(trig/iri :rdf "type")
                (trig/iri :ontology "PopulationCharacteristic")]
               [(trig/iri :rdfs "label")
                (trig/lit name)]
               [(trig/iri :ontology "is_measured_at") mm-uri]
               variable-rdf)
     (trig/iri :ontology "has_result_property")
     (map #(trig/iri :ontology %) (:properties result-properties)))))

(defn find-baseline-groups
  [xml]
  (let [groups-xml (vtd/search xml "/result/baselineCharacteristics/baselineReportingGroups/baselineReportingGroup")]
    (map (fn [%] {:id          (vtd/attr % "id")
                  :arm-id      (vtd/attr % "armId")
                  :sample-size (lib/parse-int (lib/text-at % "subjects"))
                  :description (lib/text-at % "description")})
         groups-xml)))

(defn find-arms
  [xml]
  (let [arms-xml (vtd/search xml "/result/subjectDisposition/postAssignmentPeriods/postAssignmentPeriod/arms/arm")]
    (map (fn [%] {:id          (vtd/attr % "id")
                  :title       (lib/text-at % "title")
                  :sample-size (lib/parse-int (lib/text-at % "startedMilestoneAchievement/subjects"))
                  :description (lib/text-at % "description")})
         arms-xml)))

(defn find-adverse-event-groups
  [xml]
  (let [groups-xml (vtd/search xml "/result/adverseEvents/reportingGroups/reportingGroup")]
    (map (fn [%] {:id          (vtd/attr % "id")
                  :title       (lib/text-at % "title")
                  :sample-size (lib/parse-int (lib/text-at % "subjectsExposed"))
                  :description (lib/text-at % "description")})
         groups-xml)))

(defn build-group-uris
  [{arms                 :arms
    baseline-groups      :baseline-groups
    adverse-event-groups :adverse-event-groups}]
  (let [group-uris-by-id          (into {}
                                        (map #(vector (:id %) (lib/gen-uri))
                                             (concat arms adverse-event-groups)))
        baseline-group-uris-by-id (into {}
                                        (map #(vector (:id %) 
                                                      (group-uris-by-id (:arm-id %)))
                                             baseline-groups))]
    (merge group-uris-by-id baseline-group-uris-by-id)))

(defn find-groups
  [xml]
  {:arms                 (find-arms xml)
   :baseline-groups      (find-baseline-groups xml)
   :adverse-event-groups (find-adverse-event-groups xml)})

(defn build-groups-with-uris
  [{arms                 :arms
    baseline-groups      :baseline-groups
    adverse-event-groups :adverse-event-groups}
   group-uris]
  (let [add-uri (fn [group]
                  (merge {:uri (group-uris (:id group))}
                         group))]
    {:arms                 (map add-uri arms)
     :baseline-groups      (map add-uri baseline-groups)
     :adverse-event-groups (map add-uri adverse-event-groups)
     :overall {:title "Overall population"
               :uri (lib/gen-uri)}}))

(defn build-groups-rdf
  [{arms                 :arms
    baseline-groups      :baseline-groups
    adverse-event-groups :adverse-event-groups
    overall :overall}]
  (let [non-arm-baseline-groups (filter #(not (:arm-id %)) baseline-groups)]
    (concat
     (map #(lib/arm-rdf (:uri %) %)
          arms)
     (map #(lib/group-rdf (:uri %) %)
          (concat non-arm-baseline-groups adverse-event-groups))
     [(lib/overall-population-rdf (:uri overall) overall)])))

; FIXME: countable values?
(defn read-endpoint-measurement 
; assumes xml is at .../endpoint/armReportingGroups/armReportingGroup
  [xml]
  {:arm-id           (vtd/attr xml "armId")
   :tendency-value   (lib/parse-double (lib/text-at xml "tendencyValues/tendencyValue/value"))
   :dispersion-value (lib/parse-double (lib/text-at xml "dispersionValues/dispersionValue/value"))
   :high-range-value (lib/parse-double (lib/text-at xml "./dispersionValues/dispersionValue/highRangeValue"))
   :sample-size      (lib/parse-int (lib/text-at xml "subjects"))})

(defn build-continuous-measurement-rdf
  [measurement result-properties variable-uri mm-uri group-uris sample-size]
  (as-> measurement meas
    (lib/measurement-meta-rdf (lib/gen-uri)
                              variable-uri
                              (group-uris (:arm-id meas))
                              mm-uri)
    (trig/spo meas
              [(trig/iri :ontology "sample_size")
               (trig/lit sample-size)])
    (if (not (nil? (:tendency result-properties)))
      (trig/spo meas
                [(trig/iri :ontology (:tendency result-properties))
                 (trig/lit (:tendency-value measurement))])
      meas)
    (reduce (fn [subj [property value]]
              (trig/spo subj [(trig/iri :ontology property)
                              (trig/lit value)]))
            meas
            (map vector
                 (:dispersion result-properties)
                 (list (:dispersion-value measurement)
                       (:high-range-value measurement))))))

(defn read-endpoint-measurements
  [xml endpoint-uri mm-uri group-uris]
  (let [results-properties (variable-results-properties xml)
        measurements-xml   (vtd/search xml "./armReportingGroups/armReportingGroup")
        has-categories?    (> 0 (count (:category-ids results-properties)))
        measurements       (map read-endpoint-measurement measurements-xml)]
    (map #(build-continuous-measurement-rdf 
           % results-properties
           endpoint-uri mm-uri group-uris (:sample-size %))
;; FIXME: figure out uniformity baseline/endpoint
         measurements)))

(defn read-adverse-event-measurement
  [xml]
  {:arm-id      (vtd/attr  xml "reportingGroupId")
   :count       (lib/parse-int (lib/text-at xml "./subjectsAffected"))
   :event-count (lib/parse-int (lib/text-at xml "./occurrences"))
   :sample-size (lib/parse-int (lib/text-at xml "subjectsExposed"))})

(defn build-dichotomous-measurement-rdf
  [measurement variable-uri mm-uri group-uris]
  (as-> measurement meas
    (lib/measurement-meta-rdf (lib/gen-uri)
                              variable-uri
                              (group-uris (:arm-id meas))
                              mm-uri)
    (trig/spo meas 
              [(trig/iri :ontology "sample_size")
               (trig/lit (:sample-size measurement))]
              [(trig/iri :ontology "count")
               (trig/lit (:count measurement))]
              [(trig/iri :ontology "event_count")
               (trig/lit (:event-count measurement))])))

(defn read-adverse-event-measurements
  [xml variable-uri mm-uri group-uris]
  (let [measurements-xml (vtd/search xml "./values/value")
        measurements     (map read-adverse-event-measurement measurements-xml)]
    (map #(build-dichotomous-measurement-rdf % variable-uri mm-uri group-uris)
         measurements)))


(defn read-group-categorical-measurement-values
  [xml group-id-tag]
  (let [countable-values         (vtd/search xml "./countableValues/countableValue")
        measurements-by-category (into {}
                                       (map #(vector (vtd/attr % "categoryId")
                                                     (lib/parse-int (lib/text-at % "value")))
                                            countable-values))]
    (merge measurements-by-category {:arm-id (vtd/attr xml group-id-tag)})))

(defn read-group-continuous-measurement-values
  [xml id-tag]
  {:arm-id           (vtd/attr xml id-tag)
   :tendency-value   (lib/parse-double
                      (lib/text-at xml "./tendencyValue/value"))
   :dispersion-value (lib/parse-double
                      (lib/text-at xml "./dispersionValue/value"))
   :high-range-value (lib/parse-double
                      (lib/text-at xml "./dispersionValue/highRangeValue"))})


(defn read-group-continuous-baseline-measurement-values
  [xml]
  (read-group-continuous-measurement-values xml "baselineReportingGroupId"))

(defn read-group-continuous-endpoint-measurement-values
  [xml]
  (read-group-continuous-measurement-values xml "armId"))

(defn build-category-count
  [[category count]]
  [(trig/iri :ontology "category_count")
   (-> (trig/_po
        [(trig/iri :ontology "category")
         category])
       (trig/spo
        [(trig/iri :ontology "count") count]))])

(defn build-categorical-measurement-rdf
  [measurement variable-uri mm-uri group-uris categories]
  (let
   [instance-uri             (lib/gen-uri)
    category-uris-and-counts (map (fn [[category-id count]]
                                    [(:uri (categories category-id)) count])
                                  (remove #(= :arm-id (first %)) measurement))
    category-count-rdf       (map build-category-count category-uris-and-counts)
    base-rdf                 (lib/measurement-meta-rdf
                              instance-uri
                              variable-uri
                              (group-uris (:arm-id measurement))
                              mm-uri)]
    (reduce #(trig/spo %1 %2)
            base-rdf
            category-count-rdf)))

(defn read-baseline-measurements-categorical
  [xml baseline-uri mm-uri group-uris categories]
  (let [reporting-groups-xml (vtd/search xml "./reportingGroups/reportingGroup")
        measurements         (map #(read-group-categorical-measurement-values % "baselineReportingGroupId")
                                  reporting-groups-xml)]
    (map #(build-categorical-measurement-rdf % baseline-uri mm-uri group-uris categories)
         measurements)))

(defn read-baseline-measurements-continuous
  [xml baseline-uri mm-uri group-uris sample-sizes]
  (let [reporting-groups-xml (vtd/search xml "./reportingGroups/reportingGroup")
        result-properties (variable-results-properties xml)
        measurements         (map read-group-continuous-baseline-measurement-values
                                  reporting-groups-xml)]
    (map #(build-continuous-measurement-rdf % result-properties
                                            baseline-uri mm-uri group-uris
                                            (sample-sizes (:arm-id %)))
         measurements)))

(defn make-category-vector
  [category-xml uri]
  (let [title (lib/text-at category-xml "name")]
    [(vtd/attr category-xml "id")
     {:uri   uri
      :title title
      :rdf   (trig/spo uri
                       [(trig/iri :rdfs "label")
                        (trig/lit title)]
                       [(trig/iri :rdf "type")
                        (trig/iri :ontology "Category")])}]))

(defn find-categories
  [xml]
  (let [category-nodes (vtd/search xml "/*//categories/category")]
    (into {}
          (map #(make-category-vector % (lib/gen-uri))
               category-nodes))))


(defn find-sample-sizes
  [groups]
  (into {} 
        (map (fn [arm] [(arm :id) (arm :sample-size)])
             (groups :arms))))

(defn read-all-baseline-measurements
  [baselines-xml baseline-mm-uri variable-uris group-uris categories sample-sizes]
  (mapcat
   (fn [baseline-xml idx]
     (if (= "categorical" (measurement-type-for-baseline (vtd/tag baseline-xml)))
       (read-baseline-measurements-categorical baseline-xml
                                               (variable-uris [:baseline idx])
                                               baseline-mm-uri
                                               group-uris
                                               categories)
       (read-baseline-measurements-continuous baseline-xml
                                              (variable-uris [:baseline idx])
                                              baseline-mm-uri
                                              group-uris
                                              sample-sizes)))
   baselines-xml
   (iterate inc 1)))

(defn read-endpoint-measurements-categorical
  [xml endpoint-uri mm-uri group-uris categories]
  (let [reporting-groups-xml (vtd/search xml "./armReportingGroups/armReportingGroup")
        result-properties    (variable-results-properties xml)
        measurements         (map #(read-group-categorical-measurement-values % "armId")
                                  reporting-groups-xml)]
    (map #(build-categorical-measurement-rdf % endpoint-uri mm-uri group-uris
                                             categories)
         measurements)))
    
(defn measurement-type-for-endpoint
  [xml]
  (if (= "false" (lib/text-at xml "countable"))
    "continuous"
    (if (= 0 (count (vtd/search xml "./categories/category")))
      "dichotomous"
      "categorical")))

(defn read-all-endpoint-measurements
  [endpoints-xml mm-uris variable-uris group-uris categories]
  (mapcat
   (fn [endpoint-xml idx]
     (if (= "categorical" (measurement-type-for-endpoint endpoint-xml))
       (read-endpoint-measurements-categorical endpoint-xml
                                               (variable-uris [:endpoint idx])
                                               (mm-uris [:endpoint idx])
                                               group-uris
                                               categories)
       (read-endpoint-measurements endpoint-xml
                                   (variable-uris [:endpoint idx])
                                   (mm-uris [:endpoint idx])
                                   group-uris)))
   endpoints-xml
   (iterate inc 1)))

(defn read-all-event-measurements
  [events-xml mm-uri variable-uris group-uris]
  (mapcat #(read-adverse-event-measurements %1
                                            (variable-uris [:event %2])
                                            mm-uri group-uris)
          events-xml
          (iterate inc 1)))

(defn allocation-rdf
 [subj allocation-value]
 (trig/spo subj [(trig/iri :ontology "has_allocation")
                 (trig/iri :ontology (ALLOCATION allocation-value))]))

(defn blinding-rdf
 [subj blinding-value]
 (trig/spo subj [(trig/iri :ontology "has_blinding")
                    (trig/iri :ontology (BLINDING_TYPE blinding-value))]))

(defn create-study-node
  [xml reg-uri baseline-uris endpoint-uris event-uris groups-with-uris]
  (-> (lib/gen-uri)
      (trig/spo [(trig/iri :ontology "has_publication") reg-uri]
                [(trig/iri :rdf "type") (trig/iri :ontology "Study")]
                [(trig/iri :rdfs "label")
                 (trig/lit (lib/text-at xml "/result/trialInformation/fullTitle"))]
                [(trig/iri :rdfs "comment")
                 (trig/lit (lib/text-at xml "/result/trialInformation/fullTitle"))]
                [(trig/iri :ontology "has_objective")
                 (trig/_po [(trig/iri :rdfs "comment")
                            (trig/lit (lib/text-at xml "/result/trialInformation/mainObjective"))])]
                [(trig/iri :ontology "has_eligibility_criteria")
                 (trig/_po [(trig/iri :rdfs "comment")
                            (trig/lit
                             (lib/text-at xml
                                          "/result/subjectDisposition/screeningInformation"))])])

      (allocation-rdf (lib/text-at xml "/result/subjectDisposition/postAssignmentPeriods/postAssignmentPeriod/allocation/value"))
      (blinding-rdf (lib/text-at xml "/result/subjectDisposition/postAssignmentPeriods/postAssignmentPeriod/blindingType/value"))
      (lib/spo-each (trig/iri :ontology "has_outcome") (vals baseline-uris))
      (lib/spo-each (trig/iri :ontology "has_outcome") (vals endpoint-uris))
      (lib/spo-each (trig/iri :ontology "has_outcome") (vals event-uris))
      (lib/spo-each (trig/iri :ontology "has_arm") 
                    (map :uri (:arms groups-with-uris)))
      (lib/spo-each (trig/iri :ontology "has_group") 
                    (map :uri (filter #(not (:arm-id %))
                                      (:baseline-groups groups-with-uris))))
      (lib/spo-each (trig/iri :ontology "has_group") 
                    (map :uri (:adverse-event-groups groups-with-uris)))
      (trig/spo [(trig/iri :ontology "has_included_population") 
                 (:uri (:overall groups-with-uris))])))

(defn build-variable-uris
  [xmls key]
  (into {}
        (map #(vector [key %2] (trig/iri :instance (lib/uuid)))
             xmls
             (iterate inc 1))))

(defn import-eudract
  [xml]
  (let [eudract-id        (get-eudract-number xml)
        nct-id            (get-nct-id xml)
        uri               (trig/iri :study eudract-id)
        reg-uri           (trig/iri :ictrp eudract-id)
        registration      (build-registration reg-uri eudract-id)
        [mm-uris mm-info] (find-measurement-moments xml)
        groups            (find-groups xml)
        group-uris        (build-group-uris groups)
        groups-with-uris  (build-groups-with-uris groups group-uris)
        categories        (find-categories xml)
        category-rdf      (map :rdf (vals categories))
        endpoints-xml     (find-endpoints-xml xml)
        endpoint-uris     (build-variable-uris endpoints-xml :endpoint)
        endpoints-rdf     (map #(endpoint-rdf %1 %2 endpoint-uris mm-uris)
                               endpoints-xml
                               (iterate inc 1))
        events-xml        (find-adverse-events-xml xml)
        event-uris        (build-variable-uris events-xml :event)
        events-rdf        (map #(adverse-event-rdf %1 %2 event-uris mm-uris)
                               events-xml
                               (iterate inc 1))
        baselines-xml     (find-baseline-xml xml)
        baseline-uris     (build-variable-uris baselines-xml :baseline)
        baseline-rdf      (map #(baseline-var-rdf %1 %2 baseline-uris mm-uris categories)
                               baselines-xml
                               (iterate inc 1))
        groups-rdf        (build-groups-rdf groups-with-uris)
        mms-rdf           (map #(lib/mm-rdf (first %) (second %)) mm-info)
        sample-sizes      (find-sample-sizes groups)
        measurements-rdf  (concat
                           (read-all-endpoint-measurements
                            endpoints-xml mm-uris endpoint-uris group-uris categories)
                           (read-all-baseline-measurements
                            baselines-xml
                            (mm-uris [:baseline])
                            baseline-uris group-uris categories sample-sizes)
                           (read-all-event-measurements
                            events-xml
                            (mm-uris [:events])
                            event-uris group-uris))
        study-rdf         (create-study-node xml reg-uri baseline-uris
                                             endpoint-uris event-uris groups-with-uris)
        triples           (concat [study-rdf registration] mms-rdf baseline-rdf
                                  category-rdf endpoints-rdf events-rdf groups-rdf
                                  measurements-rdf)]
    (trig/write-ttl lib/prefixes triples)))
