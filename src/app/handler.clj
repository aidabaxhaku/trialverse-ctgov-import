(ns app.handler
  (:require [app.ctgov-import :refer [import-xml]]
            [app.eudract-import :refer [import-eudract]]
            [riveted.core :as vtd]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.util.codec :refer [url-encode]]
            [clj-http.client :as client]
            [clojure.data.json :as json])
  (:use [clojusc.ring.xml :only [wrap-xml-request]]
        [clojure.data.xml :only [emit-str]]))

(defn get-from-ctgov
  [id xml-type]
  (:body
    (client/get
      (str "https://clinicaltrials.gov/show/" (url-encode id) "?" xml-type "=TRUE"))))

(defn do-ctgov-import
  [id]
  (import-xml (vtd/navigator (get-from-ctgov id "resultsXml"))))

(defn do-eudract-import
  [xml]
  (import-eudract (vtd/navigator (emit-str xml))))

(defn normalize-date
  [date-str]
  (.format
    (java.text.SimpleDateFormat. "yyyy-MM-dd")
    (.parse (java.text.SimpleDateFormat. "MMM dd, yyyy") date-str)))

(defn basic-info
  [id]
  (let [xml (vtd/navigator (get-from-ctgov id "displayXml"))
        canonical-id (vtd/text (vtd/at xml "/clinical_study/id_info/nct_id"))]
    {:id canonical-id
     :aliases (map vtd/text (vtd/search xml "/clinical_study/id_info/nct_alias"))
     :title (vtd/text (vtd/at xml "/clinical_study/brief_title"))
     :created_at (normalize-date (vtd/text (vtd/at xml "/clinical_study/study_first_submitted")))
     :updated_at (normalize-date (vtd/text (vtd/at xml "/clinical_study/last_update_submitted")))
     :sponsor (vtd/text (vtd/at xml "/clinical_study/sponsors/lead_sponsor/agency"))
     :_links {:source {:title (vtd/text (vtd/at xml "/clinical_study/required_header/link_text"))
                       :href (vtd/text (vtd/at xml "/clinical_study/required_header/url"))}
              :self {:href (str "/" canonical-id)}
              :rdf {:href (str "/" canonical-id "/rdf")}}}))


(defn wrap-404
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        (if (= 404 (:status (ex-data e)))
          {:status 404 :body (.getMessage e)}
          (throw e))))))

(defroutes app-routes
  (GET "/" [] {:status 200
                 :body   "Go to /NCTXXXXXXXX."})
  (GET "/:id{NCT[0-9]+}" [id] {:status  200
                               :headers {"Content-Type" "application/json"}
                               :body    (json/write-str (basic-info id))})
  (GET "/:id{NCT[0-9]+}/rdf" [id] {:status  200
                                   :headers {"Content-Type" "text/turtle"}
                                   :body    (do-ctgov-import id)})
  (wrap-xml-request (POST "/eudract"
                      params
                      {:status  200
                       :headers {"Content-Type" "text/turtle"}
                       :body    (do-eudract-import (:body params))}))
  (route/not-found "Not Found"))

(def app
  (wrap-404 (handler/site app-routes)))
