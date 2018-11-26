(ns app.core
  (:require
    [clojure.java.io :refer [as-file]]
    [app.ctgov-import :as ctgov]
    [app.eudract-import :as eudract]
    [riveted.core :as vtd]))

(defn -main
  [& args]
  (let [data (vtd/navigator (slurp (as-file (first args))))]
    (println (ctgov/import-xml data))))
