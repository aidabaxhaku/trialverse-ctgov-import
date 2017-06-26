(ns app.core-test
  (:require [riveted.core :as vtd]
            [org.drugis.addis.rdf.trig :as trig])
  (:use clojure.test)
  (:use app.core))

(def baseline-xml-str "<measure>
<title>Age</title>
<units>Participants</units>
<param>Count of Participants</param>
<class_list>
<class>
<category_list>
<category>
<title>under 18</title>
<measurement_list>
<measurement group_id=\"B1\" value=\"0\"/>
<measurement group_id=\"B2\" value=\"0\"/>
<measurement group_id=\"B3\" value=\"0\"/>
</measurement_list>
</category>
<category>
<title>Between 18 and 65 years</title>
<measurement_list>
<measurement group_id=\"B1\" value=\"14\"/>
<measurement group_id=\"B2\" value=\"14\"/>
<measurement group_id=\"B3\" value=\"28\"/>
</measurement_list>
</category>
<category>
<title>over 65 years</title>
<measurement_list>
<measurement group_id=\"B1\" value=\"0\"/>
<measurement group_id=\"B2\" value=\"0\"/>
<measurement group_id=\"B3\" value=\"0\"/>
</measurement_list>
</category>
</category_list>
</class>
</class_list>
</measure>")

(def category-uris (list "category-uri"))

(def baseline-xml (vtd/navigator baseline-xml-str))

(def subj (trig/iri "http://subject.com"))

(deftest baseline-measurement-properties-test
  (is (= (.toString (baseline-measurement-properties baseline-xml))
         (str "{:categories (\"under 18\" \"Between 18 and 65 years\" \"over 65 years\"), :simple false, "
            ":param \"Count of Participants\", :dispersion nil, :units \"Participants\"}"))))

(deftest measurement-meta-rdf-test
  (is (= (measurement-meta-rdf subj "outcome-uri" "group-uri" "mm-uri")
         [[:uri "http://subject.com"]
          (list
           [[:qname :ontology "of_outcome"] [:lit "outcome-uri"]]
           [[:qname :ontology "of_group"] [:lit "group-uri"]]
           [[:qname :ontology "of_moment"] [:lit "mm-uri"]])])))

(deftest baseline-measurement-data-rdf-test
  (is (= (baseline-measurement-data-rdf subj baseline-xml baseline-xml "group-uri" category-uris)
         ())))

; (deftest baseline-measurements-test
;   (is
;     (= (let
;          [idx 1
;           sample-size-xml '()
;           baseline-uris (list "baseline-uri")
;           group-uris (list "group-uri")
;           mm-uris (list "mm-uri")
;           result (baseline-measurements baseline-xml idx baseline-xml baseline-uris group-uris mm-uris category-uris)]
;          result)

;                                       false)))
