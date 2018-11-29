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

(defn spo-each [subj pred obj*]
  (reduce (fn [subj obj] (trig/spo subj [pred obj])) subj obj*))

(defn blinding-rdf [subj blinding]
  (if blinding
    (trig/spo subj [(trig/iri :ontology "has_blinding")
                    (trig/iri :ontology (clojure.string/replace (first blinding) " " ""))])
    subj))

(defn assign-uri-to-cluster
  [cluster]
  (let [uri (trig/iri :instance (uuid))]
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
