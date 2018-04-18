(defproject ctgov-import "0.1.5"
  :description "Converter from ClinicalTrials.gov XML to ADDIS-compatible RDF (text/turtle)"
  :url "https://github.com/gertvv/trialverse-ctgov-import"
  :license {:name "GNU GPL-3"
            :url "https://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[clj-http "2.1.0"]
                 [org.clojure/clojure "1.9.0"]
                 [riveted "0.1.0"]
                 [instaparse "1.4.1"]
                 [org.drugis.addis/rdfexport "1.0.1"]
                 [compojure "1.3.1"]
                 [ring/ring-defaults "0.3.1"]
                 [org.clojure/data.json "0.2.6"]]
  :repositories [["drugis.org" "https://drugis.org/mvn"]]
  :plugins [[lein-ring "0.12.4"]]
  :ring {:handler app.handler/app}
  :main app.core
  :profiles {:uberjar {:aot :all}})
