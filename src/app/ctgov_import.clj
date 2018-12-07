(ns app.ctgov-import
  (:require     
    [app.import-shared :as lib]
    [clojure.string :refer [lower-case]]
    [riveted.core :as vtd]
    [app.design-parse :refer [parse-masking]]
    [clojure.set :refer [map-invert]]
    [org.drugis.addis.rdf.trig :as trig]  ))

(defn row-label-sample-size
  [label]
  (let [m1 (second (re-find #"(?i)\W(n\s*=\s*(\d+)([,;](\s*n\s*=)?\s*(\d+))*)" label))
        m2 (if m1 (re-seq #"\d+" m1) [])
        s (map #(Integer/parseInt %) (remove nil? m2))]
    s))

(defn measurement-row-info
  [xml row-title]
  (let [group-ids         (map #(vtd/attr % "group_id")
                               (vtd/search xml "./group_list/group"))
        sample-size-guess (row-label-sample-size row-title)
        sample-size       (if (and sample-size-guess
                                   (= (count sample-size-guess)
                                      (count group-ids)))
                            (zipmap group-ids sample-size-guess)
                            {})]
    {:title       row-title
     :sample-size sample-size}))

; probe the outcome for measurement properties
(defn outcome-measurement-properties
  [xml]
  (let [measures-xml (vtd/search xml ".//measure")
        measure-xml (last measures-xml)
        categories-xml (vtd/at measure-xml "./*//category_list")
        category-count (count (vtd/children categories-xml))
        category-info (map #(measurement-row-info xml (vtd/text %)) 
                           (vtd/search categories-xml "./category/title"))
        ; probe the measure for type: <param> and <dispersion>, plus <units>
        param (vtd/text (vtd/at measure-xml "./param"))
        dispersion (vtd/text (vtd/at measure-xml "./dispersion"))
        units (vtd/text (vtd/at measure-xml "./units"))]
      { :simple (= 1 category-count)
        :categories category-info
        :param param
        :dispersion dispersion
        :units units}))

(defn baseline-measurement-properties
  [measure-xml]
  (let [categories-xml (vtd/at measure-xml ".//category_list")
        category-titles (concat (map vtd/text (vtd/search categories-xml "./category/sub_title"))
                                (map vtd/text (vtd/search categories-xml "./category/title")))
        category-titles (if (> (count category-titles) 0) ; fallback for incorrect cases where title is not set on categories
                          category-titles
                          (map vtd/text (vtd/search measure-xml ".//class/title")))
        param (vtd/text (vtd/at measure-xml "./param"))
        dispersion (vtd/text (vtd/at measure-xml "./dispersion"))
        units (vtd/text (vtd/at measure-xml "./units"))]
    { :categories category-titles
      :simple (empty? category-titles)
      :param param
      :dispersion dispersion
      :units units }))



(defn is-proportion-outcome
  [props]
  (and (= "Number" 
          (:param props)) 
       (lib/string-starts-with-any? (lower-case (:units props)) ["proportion"])))

(defn is-percent-outcome
  [props]
  (and (= "Number" 
          (:param props)) 
       (lib/string-starts-with-any? (lower-case (:units props)) ["percent" "percentage" "percentages" "%" "observed percentage"])))

(defn is-count-outcome
  [props]
  (and
    (or (= "Number" (:param props)) (= "Count of Participants" (:param props)))
    (not (is-percent-outcome props)) (not (is-proportion-outcome props))))

; determine results properties from the measurement properties
(defn outcome-results-properties
  [props]
  (concat
    (if (= "Mean" (:param props)) {"mean" "value"})
    (if (= "Median" (:param props)) {"median" "value"})
    (if (is-count-outcome props) {"count" "value"})
    (if (is-percent-outcome props) {"percentage" "value"}) ; FIXME: add to ontology?
    (if (is-proportion-outcome props) {"proportion" "value"}) ; FIXME: add to ontology?
    (if (= "Geometric Mean" (:param props)) {"geometric_mean" "value"}) ; FIXME: add to ontology?
    (if (= "Log Mean" (:param props)) {"log_mean" "value"}) ; FIXME: add to ontology?
    (if (= "Least Squares Mean" (:param props)) {"least_squares_mean" "value"}) ; FIXME: add to ontology?
    (if (= "90% Confidence Interval" (:dispersion props)) {"quantile_0.05" "lower_limit"
                                                           "quantile_0.95" "upper_limit"})
    (if (= "95% Confidence Interval" (:dispersion props)) {"quantile_0.025" "lower_limit"
                                                           "quantile_0.975" "upper_limit"})
    (if (= "Full Range" (:dispersion props)) {"min" "lower_limit"
                                              "max" "upper_limit"})
    (if (= "Geometric Coefficient of Variation" (:dispersion props)) {"geometric_coefficient_of_variation" "spread"}) ; FIXME: add to ontology?
    (if (= "Inter-Quartile Range" (:dispersion props)) {"first_quartile" "lower_limit"
                                                        "third_quartile" "upper_limit"})
    (if (= "Standard Deviation" (:dispersion props)) {"standard_deviation" "spread"})
    (if (= "Standard Error" (:dispersion props)) {"standard_error" "spread"})))

(defn outcome-measurement-type
  [param]
  (if (= "Number" param) "dichotomous" "continuous"))

(defn outcome-rdf
  [xml idx outcome-uris mm-uris]
  (let [uri (outcome-uris [:outcome idx])
        props (outcome-measurement-properties xml)
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
                            (trig/iri :ontology (outcome-measurement-type (:param props)))])])
      (trig/iri :ontology "has_result_property")
        (map #(trig/iri :ontology %) (keys properties)))))

(defn baseline-var-type
  [{categories :categories
    param :param}]
  (if (= 0 (count categories))
    [nil (trig/_po [(trig/iri :ontology "measurementType")
                    (trig/iri :ontology (outcome-measurement-type param))])]
    (if (or (= "Number" param) 
            (= "Count of Participants" param))
      (let [category-uris (into {}
                                (map #(vector % (trig/iri :instance (lib/uuid)))
                                     categories))
            category-rdfs (map #(trig/spo
                                 (second %)
                                 [(trig/iri :rdfs "label") (trig/lit (first %))]
                                 [(trig/iri :rdf "type") (trig/iri :ontology "Category")])
                               category-uris)
            categories {:uris category-uris
                        :rdfs category-rdfs}]
        [categories (trig/_po [(trig/iri :ontology "measurementType") 
                               (trig/iri :ontology "categorical")]
                              [(trig/iri :ontology "categoryList") 
                               (trig/coll (vals (:uris categories)))])])
      [nil (trig/_po)])))

(defn baseline-var-rdf
  [xml idx baseline-uris mm-uris]
  (let [uri                  (baseline-uris idx)
        var-name             (vtd/text (vtd/at xml "./title"))
        props                (baseline-measurement-properties xml)
        properties           (outcome-results-properties props)
        [categories var-rdf] (baseline-var-type props)
        subj                 (trig/spo 
                              uri
                              [(trig/iri :rdf "type") (trig/iri :ontology "PopulationCharacteristic")]
                              [(trig/iri :rdfs "label") (trig/lit var-name)]
                              [(trig/iri :ontology "is_measured_at")
                               (mm-uris [:baseline])]
                              [(trig/iri :ontology "of_variable") var-rdf])]
    [categories
     (if categories
       (-> subj
           (trig/spo [(trig/iri :ontology "has_result_property") 
                      (trig/iri :ontology "sample_size")])
           (lib/spo-each (trig/iri :ontology "has_result_property")
                         (map #(trig/iri :ontology %) (keys properties))))
       subj)]))

(defn adverse-event-rdf
  [xml idx event-uris mm-uris]
  (let [uri (event-uris idx)
        event-name (vtd/text (vtd/at xml "./sub_title"))
        group-name (vtd/text (vtd/at xml "../../title"))]
    (lib/spo-each
      (trig/spo uri
                [(trig/iri :rdf "type") (trig/iri :ontology "AdverseEvent")]
                [(trig/iri :rdfs "label") (trig/lit event-name)]
                [(trig/iri :rdfs "comment") (trig/lit (str group-name ", " event-name))]
                [(trig/iri :ontology "is_measured_at") (mm-uris [:events])]
                [(trig/iri :ontology "of_variable")
                 (trig/_po [(trig/iri :ontology "measurementType") 
                            (trig/iri :ontology "dichotomous")])])
      (trig/iri :ontology "has_result_property")
      (map #(trig/iri :ontology %) ["sample_size" "count" "event_count"]))))

(defn find-arm-groups
  [xml]
  (into {} (map (fn [ag idx] [[:arm_group idx]
                              {:title (vtd/text (vtd/at ag "./arm_group_label"))
                               :description (or (vtd/text (vtd/at ag "./description")) "")}])
                (vtd/search xml "/clinical_study/arm_group")
                (iterate inc 1))))

(defn group-info
  [group-xml]
  {:title (vtd/text (vtd/at group-xml "./title"))
   :description (or (vtd/text (vtd/at group-xml "./description")) "")})

(defn find-baseline-groups
  [xml]
  (into {} (map (fn [group] [[:baseline_group (vtd/attr group :group_id)]
                             (group-info group)])
                (vtd/search xml "/clinical_study/clinical_results/baseline/group_list/group"))))

(defn find-event-groups
  [xml]
  (into {} (map (fn [group] [[:events_group (vtd/attr group :group_id)]
                             (group-info group)])
                (vtd/search xml "/clinical_study/clinical_results/reported_events/group_list/group"))))

(defn find-groups-for-outcome
  [outcome-xml idx]
  (into {} (map (fn [group] [[:outcome_group idx (vtd/attr group :group_id)]
                             (group-info group)])
                (vtd/search outcome-xml "./group_list/group"))))

(defn find-outcome-groups
  [xml]
  (apply merge (map find-groups-for-outcome
                    (vtd/search xml "/clinical_study/clinical_results/outcome_list/outcome")
                    (iterate inc 1))))

(defn std-group
  [group]
  {:title (lower-case (:title group))
   :description (lower-case (:description group))})

(defn find-groups
  [xml]
  (lib/sort-equivalent-values (merge
                               (find-arm-groups xml)
                               (find-baseline-groups xml)
                               (find-event-groups xml)
                               (find-outcome-groups xml))
                          std-group))

(defn find-event-time-frame
  [xml]
  { [:events] (or (vtd/text (vtd/at xml "/clinical_study/clinical_results/reported_events/time_frame")) "Unknown") })

(defn find-outcome-time-frames
  [xml]
  (into {} (map #(vector [:outcome %2] (vtd/text %1))
                (vtd/search xml "/clinical_study/clinical_results/outcome_list/outcome/time_frame")
                (iterate inc 1))))

(defn find-measurement-moments
  [xml]
  (lib/sort-equivalent-values (merge (find-event-time-frame xml)
                                 (find-outcome-time-frames xml)
                                 {[:baseline] "Baseline"})
                                 lower-case))

;    ontology:category_count [
;      ontology:category "Female" ;
;      ontology:count "43"
;    ] .

; sample-size-xml: 
; <analyzed>
;   <units>Participants</units>
;   <scope>Overall</scope>
;   <count_list>
;     <count group_id="B1" value="826"/>
;     <count group_id="B2" value="822"/>
;     <count group_id="B3" value="824"/>
;     <count group_id="B4" value="825"/>
;     <count group_id="B5" value="3297"/>
;   </count_list>
; </analyzed>

(defn measurement-data-rdf-basic
  [subj properties sample-size-xml measure-xml group-id]
  (let [measurement-query (format "./*//category_list/category/measurement_list/measurement[@group_id=\"%s\"]" group-id)
        sample-size-query (format ".//count_list/count[@group_id=\"%s\"]" group-id)
        sample-size (vtd/at sample-size-xml sample-size-query)
        measurement-xml (vtd/at measure-xml measurement-query)]
    (reduce #(lib/measurement-value %1 measurement-xml (first %2) (second %2))
            (lib/measurement-value subj sample-size "sample_size" "value")
            properties)))

(defn measurement-data-rdf-categorical
  [subj measure-xml group-id category-uris]
  (let [categories-xml (vtd/search measure-xml "./*//category_list/category")
        measurement-query (format "./measurement_list/measurement[@group_id=\"%s\"]" group-id)
        cond-count (fn [subj value] (if 
                                     value (trig/spo 
                                            subj 
                                            [(trig/iri :ontology "count") 
                                             (lib/parse-int value)]) 
                                     subj))]
    (reduce #(trig/spo 
              %1 
              [(trig/iri :ontology "category_count")
               (-> (trig/_po [(trig/iri :ontology "category") 
                              (category-uris (or 
                                              (vtd/text (vtd/at %2 "./sub_title")) 
                                              (vtd/text (vtd/at %2 "./title"))
                                              (vtd/text (vtd/at %2 "../../title"))))])
                   (cond-count (vtd/attr (vtd/at %2 measurement-query) :value)))])
            subj
            categories-xml)))

(defn measurement-data-row-rdf
  [measure-xml group-id m-meta props row-info sample-size-xml]
  (let [measurement-xml (vtd/at measure-xml (format "./*//category_list/category/sub_title[text()=\"%s\"]/../measurement_list/measurement[@group_id=\"%s\"]" (:title row-info) group-id))
        subj (trig/spo (trig/iri :instance (lib/uuid))
                       [(trig/iri :ontology "of_outcome") (:outcome m-meta)]
                       [(trig/iri :ontology "of_group") (:group m-meta)]
                       [(trig/iri :rdfs "comment") (trig/lit (:title row-info))])
        row-specific ((:sample-size row-info) group-id)
        subj-with-sample-size (if (nil? row-specific)
                                  (lib/measurement-value subj sample-size-xml "sample_size" "value")
                                  (trig/spo subj [(trig/iri :ontology "sample_size") row-specific]))
        properties (outcome-results-properties props)]
    (reduce #(lib/measurement-value %1 measurement-xml (first %2) (second %2))
            subj-with-sample-size
            properties)))

(defn measurement-data-rdf-complex
  [props sample-size-xml measure-xml group-id m-meta]
  (let [measurement-query (format "./*//category_list/category/measurement_list/measurement[@group_id=\"%s\"]" group-id)
        sample-size-query (format ".//count_list/count[@group_id=\"%s\"]" group-id)
        sample-size (vtd/at sample-size-xml sample-size-query)]
  (map #(measurement-data-row-rdf measure-xml group-id m-meta props % sample-size) (:categories props))))

(defn outcome-measurement-data-rdf
  [xml group-id m-meta]
  (let [measures-xml (vtd/at xml "./measure")
        measure-count (count (vtd/search xml "./measure"))
        sample-size-xml (vtd/at xml "./*//analyzed_list/analyzed")
        measure-xml (vtd/last-child measures-xml)
        props (outcome-measurement-properties xml)
        properties (outcome-results-properties props)]
    (if (:simple props)
      [(measurement-data-rdf-basic
         (lib/measurement-meta-rdf (trig/iri :instance (lib/uuid)) (:outcome m-meta) (:group m-meta) (:mm m-meta))
         properties sample-size-xml measure-xml group-id)]
      (measurement-data-rdf-complex props sample-size-xml measure-xml group-id m-meta))))

(defn outcome-measurements
  [xml idx outcome-uris group-uris mm-uris]
  (let [group-id-query "./*//analyzed_list/analyzed/count_list/count/@group_id"
        groups (set (map vtd/text (vtd/search xml group-id-query)))
        m-meta (into {} (map (fn [group] [group { :outcome (outcome-uris [:outcome idx])
                                          :group (group-uris [:outcome_group idx group])
                                          :mm (mm-uris [:outcome idx]) }]) groups))]
    (apply concat (map (fn [[group-id meta-info]] (outcome-measurement-data-rdf xml group-id meta-info)) m-meta))))

(defn baseline-measurement-data-rdf
  [subj measure-xml sample-size-xml group-id category-uris]
  (let [props (baseline-measurement-properties measure-xml)
        properties (outcome-results-properties props)]
    (cond
      (:simple props) (measurement-data-rdf-basic subj properties sample-size-xml measure-xml group-id)
      (:categories props) (measurement-data-rdf-categorical subj measure-xml group-id category-uris)
      :else subj)))

(defn baseline-measurements
  [xml idx sample-size-xml baseline-uris group-uris mm-uris category-uris]
  (let [group-id-query ".//category_list/category/measurement_list/measurement/@group_id"
        groups (set (map vtd/text (vtd/search xml group-id-query)))
        m-meta (into {} 
                     (map (fn [group] [group (lib/measurement-meta-rdf (trig/iri :instance (lib/uuid))
                                                                       (baseline-uris idx)
                                                                       (group-uris [:baseline_group group])
                                                                       (mm-uris [:baseline]))]) groups))]
    (map (fn [[group subj]] (baseline-measurement-data-rdf subj xml sample-size-xml group category-uris)) m-meta)))

(defn event-measurement-rdf
  [xml event-uri group-uri mm-uri]
  (let [m-meta (trig/spo (trig/iri :instance (lib/uuid))
                         [(trig/iri :ontology "of_outcome") event-uri]
                         [(trig/iri :ontology "of_group") group-uri]
                         [(trig/iri :ontology "of_moment") mm-uri])
        properties {"count" "subjects_affected"
                    "event_count" "events"
                    "sample_size" "subjects_at_risk"}]
    (reduce #(lib/measurement-value %1 xml (first %2) (second %2)) m-meta properties)))

(defn event-measurements
  [xml idx event-uris group-uris mm-uris]
  (let [group-id-query "./counts/@group_id"
        groups (set (map vtd/text (vtd/search xml group-id-query)))]
    (map #(event-measurement-rdf
            (vtd/at xml (format "./counts[@group_id=\"%s\"]" %))
            (event-uris idx)
            (group-uris [:events_group %])
            (mm-uris [:events])) groups)))

(defn allocation-rdf [subj allocation]
  (if allocation
    (trig/spo subj [(trig/iri :ontology "has_allocation")
                    (if (= "Randomized" (first allocation))
                      (trig/iri :ontology "AllocationRandomized")
                      (trig/iri :ontology "AllocationNonRandomized"))])
    subj))

(defn import-xml
  [xml]
  (let [
        nct-id (vtd/text (vtd/at xml "/clinical_study/id_info/nct_id"))
        uri (trig/iri :study nct-id)
        reg-uri (trig/iri :ictrp nct-id)
        registration (trig/spo reg-uri
                               [(trig/iri :ontology "registry") (trig/iri "http://trials.drugis.org/registries#ClinicalTrials.gov")]
                               [(trig/iri :ontology "registration_id") (trig/lit nct-id)]
                               [(trig/iri :bibo "uri") (str "https://clinicaltrials.gov/show/" nct-id)])
        [mm-uris mm-info] (find-measurement-moments xml)
        outcome-xml (vtd/search xml "/clinical_study/clinical_results/outcome_list/outcome")
        outcome-uris (into {} (map #(vector [:outcome %2] (trig/iri :instance (lib/uuid))) outcome-xml (iterate inc 1)))
        outcomes-rdf (map #(outcome-rdf %1 %2 outcome-uris mm-uris) outcome-xml (iterate inc 1))
        event-xml (vtd/search xml "/clinical_study/clinical_results/reported_events/*//category_list/category/event_list/event")
        event-uris (into {} (map #(vector %2 (trig/iri :instance (lib/uuid))) event-xml (iterate inc 1)))
        events-rdf (map #(adverse-event-rdf %1 %2 event-uris mm-uris) event-xml (iterate inc 1))
        baseline-xml (vtd/search xml "/clinical_study/clinical_results/baseline/measure_list/measure")
        baseline-sample-size-xml (vtd/at xml "/clinical_study/clinical_results/baseline/analyzed_list/analyzed")
        baseline-var-xml (rest baseline-xml)
        baseline-uris (into {} (map #(vector %2 (trig/iri :instance (lib/uuid))) 
                                    baseline-var-xml (iterate inc 1)))
        baseline-data (map #(baseline-var-rdf %1 %2 baseline-uris mm-uris) 
                           baseline-var-xml (iterate inc 1))
        baseline-rdf (map second baseline-data)
        baseline-categories-data (map first baseline-data)
        category-uris (reduce #(merge %1 (:uris %2)) {} baseline-categories-data)
        category-rdf (reduce #(concat %1 (:rdfs %2)) [] baseline-categories-data)
        [group-uris group-info] (find-groups xml)
        groups-rdf (map #(lib/group-rdf (first %) (second %)) group-info)
        mms-rdf (map #(lib/mm-rdf (first %) (second %)) mm-info)
        measurements-rdf (concat
                           (apply concat 
                                  (map #(baseline-measurements 
                                         %1 %2 
                                         baseline-sample-size-xml 
                                         baseline-uris group-uris 
                                         mm-uris category-uris) 
                                       baseline-var-xml (iterate inc 1)))
                           (apply concat 
                                  (map #(outcome-measurements 
                                         %1 %2 outcome-uris group-uris 
                                         mm-uris) 
                                       outcome-xml (iterate inc 1)))
                           (apply concat 
                                  (map #(event-measurements
                                         %1 %2 event-uris group-uris mm-uris)
                                       event-xml (iterate inc 1))))
        study-rdf (-> uri
                     (trig/spo [(trig/iri :ontology "has_publication") reg-uri]
                               [(trig/iri :rdf "type") (trig/iri :ontology "Study")]
                               [(trig/iri :rdfs "label") 
                                (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_title")))]
                               [(trig/iri :rdfs "comment") 
                                (trig/lit (vtd/text (vtd/at xml "/clinical_study/official_title")))]
                               [(trig/iri :ontology "has_objective")
                                (trig/_po [(trig/iri :rdfs "comment") 
                                           (trig/lit (vtd/text (vtd/at xml "/clinical_study/brief_summary/textblock")))])]
                               [(trig/iri :ontology "has_eligibility_criteria")
                                (trig/_po [(trig/iri :rdfs "comment") (trig/lit (vtd/text (vtd/at xml "/clinical_study/eligibility/criteria/textblock")))])])

                     (allocation-rdf (vtd/text (vtd/at xml "/clinical_study/study_design_info/allocation")))
                     (lib/blinding-rdf (parse-masking (vtd/text (vtd/at xml "/clinical_study/study_design_info/masking"))))
                     (lib/spo-each (trig/iri :ontology "has_outcome") (vals baseline-uris))
                     (lib/spo-each (trig/iri :ontology "has_outcome") (vals outcome-uris))
                     (lib/spo-each (trig/iri :ontology "has_outcome") (vals event-uris))
                     (lib/spo-each (trig/iri :ontology "has_group") (keys group-info)))
        triples (concat [study-rdf registration] mms-rdf baseline-rdf category-rdf outcomes-rdf events-rdf groups-rdf measurements-rdf)]
    (trig/write-ttl lib/prefixes triples)))
