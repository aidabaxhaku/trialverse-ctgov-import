(ns app.core
  (:require
    [clojure.java.io :refer [as-file]]
    [clojure.string :refer [lower-case]]
    [clojure.set :refer [map-invert]]
    [app.design-parse :refer [parse-masking]]
    [app.import-shared :as lib]
    [app.ctgov_import :as ctgov]
    [app.eudract_import :as eudract]
    [riveted.core :as vtd]
    [org.drugis.addis.rdf.trig :as trig]))

(defn -main
  [& args]
  (let [data (vtd/navigator (slurp (as-file (first args))))]
    (println (ctgov/import data))))
