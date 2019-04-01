(ns app.import-shared
  (:require 
    [riveted.core :as vtd]
    [clojure.string :refer [lower-case]]
    [clojure.set :refer [map-invert]]
    [org.drugis.addis.rdf.trig :as trig]))

(def prefixes {:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                  :rdfs "http://www.w3.org/2000/01/rdf-schema#"
                  :xsd "http://www.w3.org/2001/XMLSchema#"
                  :owl "http://www.w3.org/2002/07/owl#"
                  :qudt "http://qudt.org/schema/qudt#"
                  :ontology "http://trials.drugis.org/ontology#"
                  :study "http://trials.drugis.org/studies/"
                  :ictrp "http://trials.drugis.org/ictrp-id/"
                  :instance "http://trials.drugis.org/instances/"
                  :entity "http://trials.drugis.org/entities/"
                  :dc "http://purl.org/dc/elements/1.1/"
                  :bibo "http://purl.org/ontology/bibo/"})

(defn uuid [] (str (java.util.UUID/randomUUID)))
(defn gen-uri [] (trig/iri :instance (uuid)))

(defn spo-each [subj pred obj*]
  (reduce (fn [subj obj] (trig/spo subj [pred obj]))
          subj
          obj*))

(defn blinding-rdf [subj blinding]
  (if blinding
    (trig/spo subj [(trig/iri :ontology "has_blinding")
                    (trig/iri :ontology (clojure.string/replace (first blinding) " " ""))])
    subj))

(defn assign-uri-to-cluster
  [cluster]
  (let [uri (gen-uri)]
    (into {} (map #(vector % uri) cluster))))

(defn sort-equivalent-values
  [the-map std-fn]
  (let [clusters (map #(map first %)
                      (vals (group-by #(std-fn (second %)) the-map)))
        uris (apply merge (map assign-uri-to-cluster clusters))
        info (into {} (map #(vector (first %) (the-map (second %))) (map-invert uris)))]
    [uris info]))

(defn string-starts-with-any?
  [s words]
  (some #(.startsWith s %) words))

(defn build-uris-of-type
  [xml type]
  (into {}
        (map #(vector [type %2] (trig/iri :instance (uuid)))
             xml
             (iterate inc 1))))

(defn same-ignoring-order? [coll1 coll2]
  (= (set coll1)
     (set coll2)))

(defn measurement-meta-rdf
  "Generate RDF for a set of measurement coordinates"
  [subj outcome-uri group-uri mm-uri]
  (trig/spo subj
            [(trig/iri :ontology "of_outcome") outcome-uri]
            [(trig/iri :ontology "of_group") group-uri]
            [(trig/iri :ontology "of_moment") mm-uri]))

(defn text-at [xml expr] (vtd/text (vtd/at xml expr)))

(defn parse-int
  [s]
  (try (Integer. s)
       (catch Exception e
         nil)))

(defn parse-double
  [s]
  (try (Double. s)
       (catch Exception e
         nil)))

(defn measurement-value
  [subj xml prop attr]
  (let [value-str (vtd/attr xml attr)
        value     (if (or
                       (= "count" prop)
                       (= "event_count" prop)
                       (= "sample_size" prop))
                    (parse-int value-str)
                    (parse-double value-str))]
    (if value
      (trig/spo subj [(trig/iri :ontology prop) (trig/lit value)])
      subj)))

(defn group-rdf-flex-type
  [group-uri group-info group-type]
  (trig/spo group-uri
            [(trig/iri :rdfs "label") (trig/lit (:title group-info))]
            [(trig/iri :rdfs "comment") (trig/lit (:description group-info))]
            [(trig/iri :rdf "type") (trig/iri :ontology group-type)]))

(def group-rdf #(group-rdf-flex-type %1 %2 "Group"))
(def arm-rdf #(group-rdf-flex-type %1 %2 "Arm"))
(def overall-population-rdf #(group-rdf-flex-type %1 %2 "StudyPopulation"))
(defn mm-rdf
  [mm-uri mm-title]
  (trig/spo mm-uri
            [(trig/iri :rdfs "label") (trig/lit mm-title)]
            [(trig/iri :rdf "type") (trig/iri :ontology "MeasurementMoment")]))
