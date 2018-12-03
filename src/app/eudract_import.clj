(ns app.eudract-import
  (:require
   [app.import-shared :as lib]
   [riveted.core :as vtd]
   [clojure.string :refer [lower-case]]
   [app.design-parse :refer [parse-masking]]
   [org.drugis.addis.rdf.trig :as trig]))

(def CENTRAL-TENDENCY {"leastSquares" {"least_squares_mean" "value"}
                       "arithmetic"   {"mean" "value"}
                       "geometric"    {"geometric_mean" "value"}
                       "log"          {"log_mean" "value"}
                       "median"       {"median" "value"}})

(def DISPERSION {"standardDeviation"             {"standard_deviation" "spread"}
                 "standardError"                 {"standard_error" "spread"}
                 "interQuartileRange"            {"first_quartile" "lower_limit"
                                                  "third_quartile" "upper_limit"}
                 "geometricCoefficientVariation" {"geometric_coefficient_of_variation" "spread"}
                 "fullRange"                     {"min" "lower_limit"
                                                  "max" "upper_limit"}})

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
    (vtd/text (vtd/at xml "/result/adverseEvents/timeFrame"))
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
  (vtd/text (vtd/at xml "/result/@eudractNumber")))

(defn get-nct-id
  [xml]
  (vtd/text (vtd/at xml "/result/trialInformation/usctnIdentifier")))

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

(defn measurement-row-info [] '()) ; FIXME

(defn outcome-measurement-properties
  [xml]
  (let [categories-xml (vtd/at xml "categories")
        category-count (count (vtd/children categories-xml))
        category-info  (map #(measurement-row-info xml (vtd/text %))
                            (vtd/search categories-xml "./category/name"))
        category-xml   (vtd/first-child categories-xml)
        ; probe the measure for type: <param> and <dispersion>, plus <units>
        param          (vtd/text (vtd/at xml "centralTendencyType/value"))
        dispersion     (vtd/text (vtd/at xml "dispersionType/value"))
        units          (vtd/text (vtd/at xml "unit"))]
    {:simple     (< category-count 2)
     :is-count?  (= "true" (vtd/text (vtd/at xml "countable")))
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

; determine results properties from the measurement properties
; FIXME: CIs/quantiles
(defn outcome-results-properties
  [props]
  (let [found-parameter   (concat
                           (MEASURE-TYPES (:param props))
                           (CENTRAL-TENDENCY-TYPES (:param props))
                           (if (:is-count? props) {"count" "value"})
                           (if (is-percentage? props) {"percentage" "value"})
                           (if (is-proportion? props) {"proportion" "value"}))
        found-dispersion  (concat
                           (ENDPOINT-DISPERSION-TYPES (:dispersion props))
                           (DISPERSION-TYPES (:dispersion props)))]
    (concat found-parameter found-dispersion)))

(defn outcome-measurement-type
  [props]
  (if (= "MEASURE_TYPE.number" (:param props)) "dichotomous" "continuous"))

  (defn outcome-rdf
    [xml idx outcome-uris mm-uris]
    (let [uri        (outcome-uris [:outcome idx])
          props      (outcome-measurement-properties xml)
          properties (outcome-results-properties props)]
      (lib/spo-each
       (trig/spo uri
                 [(trig/iri :rdf "type") (trig/iri :ontology "Endpoint")]
                 [(trig/iri :rdfs "label") (trig/lit (vtd/text (vtd/at xml "./title")))]
                 [(trig/iri :rdfs "comment") (trig/lit (or (vtd/text (vtd/at xml "./description")) ""))]
                 [(trig/iri :ontology "is_measured_at") (mm-uris [:outcome idx])]
                 [(trig/iri :ontology "has_result_property") (trig/iri :ontology "sample_size")]
                 [(trig/iri :ontology "of_variable")
                  (trig/_po [(trig/iri :ontology "measurementType") 
                             (trig/iri :ontology (outcome-measurement-type props))])])
       (trig/iri :ontology "has_result_property")
       (map #(trig/iri :ontology %) (keys properties)))))

(defn find-adverse-events
  [xml] 
  (concat (vtd/search xml "/result/adverseEvents/nonSeriousAdverseEvents/nonSeriousAdverseEvent")
          (vtd/search xml "/result/adverseEvents/seriousAdverseEvents/seriousAdverseEvent")))


(defn adverse-event-rdf
  [xml idx event-uris mm-uris]
  (let [uri        (event-uris [:event idx])
        event-name (vtd/text (vtd/at xml "./term"))]
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

(defn baseline-var-rdf-shared
  [xml idx uri mm-uri]
  (let [characteristic-name (vtd/text (vtd/at xml "./title"))
   measurement-type    (measurement-type-for-baseline (vtd/tag xml))
   props               (outcome-measurement-properties xml)
   properties          (outcome-results-properties props)]
  (trig/spo uri
            [(trig/iri :rdf "type")
             (trig/iri :ontology "PopulationCharacteristic")]
            [(trig/iri :rdfs "label")
             (trig/lit characteristic-name)]
            [(trig/iri :ontology "is_measured_at") mm-uri]
            [(trig/iri :ontology "of_variable")
             (trig/_po [(trig/iri :ontology "measurementType")
                        (trig/iri :ontology measurement-type)])])))

(defn baseline-var-rdf-categorical
  [xml idx baseline-uris mm-uris]
  (let [uri                 (baseline-uris [:baseline idx])
        mm-uri (mm-uris [:baseline])
        shared-rdf (baseline-var-rdf-shared xml idx uri mm-uri)]
    ))



(defn baseline-var-rdf-continuous
  [xml idx baseline-uris mm-uris]
  (let [uri        (baseline-uris [:baseline idx])
        mm-uri     (mm-uris [:baseline])
        shared-rdf (baseline-var-rdf-shared xml idx uri mm-uri)
        props               (outcome-measurement-properties xml)
        properties          (outcome-results-properties props)]
    (lib/spo-each
     shared-rdf
     (trig/iri :ontology "has_result_property")
     (map #(trig/iri :ontology %) (keys properties)))))

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
;         measurements-rdf (concat
;                            (apply concat (map #(baseline-measurements %1 %2 baseline-sample-size-xml baseline-uris group-uris mm-uris category-uris) baseline-var-xml (iterate inc 1)))
;                            (apply concat (map #(outcome-measurements %1 %2 outcome-uris group-uris mm-uris) outcome-xml (iterate inc 1)))
;                            (apply concat (map #(event-measurements %1 %2 event-uris group-uris mm-uris) event-xml (iterate inc 1))))
;         study-rdf (-> uri
;                      (trig/spo [(trig/iri :ontology "has_publication") reg-uri]
;                                [(trig/iri :rdf "type") (trig/iri :ontology "Study")]
;                                [(trig/iri :rdfs "label") (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_title")))]
;                                [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/official_title")))]
;                                [(trig/iri :ontology "has_objective")
;                                 (trig/_po [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_summary/textblock")))])]
;                                [(trig/iri :ontology "has_eligibility_criteria")
;                                 (trig/_po [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/eligibility/criteria/textblock")))])])

;                      (allocation-rdf (vtd/text (vtd/at xml "/clinical_study/study_design_info/allocation")))
;                      (blinding-rdf (parse-masking (vtd/text (vtd/at xml "/clinical_study/study_design_info/masking"))))
;                      (lib/spo-each (trig/iri :ontology "has_outcome") (vals baseline-uris))
;                      (lib/spo-each (trig/iri :ontology "has_outcome") (vals outcome-uris))
;                      (lib/spo-each (trig/iri :ontology "has_outcome") (vals event-uris))
;                      (lib/spo-each (trig/iri :ontology "has_group") (keys group-info)))
;         triples (concat [study-rdf registration] mms-rdf baseline-rdf category-rdf outcomes-rdf events-rdf groups-rdf measurements-rdf)]
;     (trig/write-ttl lib/prefixes triples)))
