(defproject ctgov-import "0.2.0"
  :description "Converter from ClinicalTrials.gov XML and EudraCT XML to ADDIS-compatible RDF (text/turtle)"
  :url "https://github.com/drugis/trialverse-ctgov-import"
  :license {:name "GNU GPL-3"
            :url  "https://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[clj-http "3.9.1"]
                 [org.clojure/clojure "1.9.0"]
                 [riveted "0.1.1"]
                 [instaparse "1.4.9"]
                 [org.drugis.addis/rdfexport "1.1.2"]
                 [org.clojure/tools.cli "0.4.1"]
                 [compojure "1.6.1"]
                 [ring/ring-defaults "0.3.2"]
                 [clojusc/ring-xml "0.1.1"]
                 [org.clojure/data.json "0.2.6"]]
  :repositories [["drugis.org" "https://drugis.org/mvn"]]
  :plugins [[lein-ancient "0.6.15"]
            [lein-auto "0.1.3"]
            [lein-ring "0.12.5"]]
  :ring {:handler app.handler/app}
  :main app.core
  :aliases {"kaocha" ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"]}
  :profiles {:uberjar {:aot :all}
             :kaocha  {:dependencies [[lambdaisland/kaocha "0.0-389"]]}})
