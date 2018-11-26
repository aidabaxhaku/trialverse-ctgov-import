(ns app.eudract-import
  (:require
   [app.import-shared :as lib]
   [riveted.core :as vtd]
   [clojure.string :refer [lower-case]]
   [app.design-parse :refer [parse-masking]]
   [org.drugis.addis.rdf.trig :as trig]))

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

(defn build-outcome-uris
  [xml] 
  (into {} (map 
            #(vector [:outcome %2] (trig/iri :instance (lib/uuid)))
            xml
            (iterate inc 1))))
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
;         outcomes-rdf (map #(outcome-rdf %1 %2 outcome-uris mm-uris) outcome-xml (iterate inc 1))
;         event-xml (vtd/search xml "/clinical_study/clinical_results/reported_events/*//category_list/category/event_list/event")
;         event-uris (into {} (map #(vector %2 (trig/iri :instance (lib/uuid))) event-xml (iterate inc 1)))
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
