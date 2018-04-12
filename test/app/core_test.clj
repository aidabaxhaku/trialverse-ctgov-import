(ns app.core-test
  (:require [riveted.core :as vtd]
            [org.drugis.addis.rdf.trig :as trig])
  (:use clojure.test)
  (:use app.core))

; (deftest test-sort-equivalent-values
;   (is (= {}
;          (sort-equivalent-values
;            {:a 1 :b 2 :c 2}
;            identity ))))

(def time-frame-xml-str
  "<clinical_study>
    <clinical_results>
      <outcome_list>
        <outcome>
          <time_frame>19 months</time_frame>
        </outcome>
        <outcome>
          <time_frame>Measured at Month 12</time_frame>
        </outcome>
      </outcome_list>
      <reported_events>
        <time_frame>
          12 months (The course of treatment for each participant in the trial)
        </time_frame>
      </reported_events>
    </clinical_results>
  </clinical_study>")

(def time_frame-xml (vtd/navigator time-frame-xml-str))

; (deftest test-find-measurement-moments
;   (is (= (find-measurement-moments time_frame-xml)
;         "something")))


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

; (deftest baseline-measurement-properties-test
;   (is (= (.toString (baseline-measurement-properties baseline-xml))
;          (str "{:categories (\"under 18\" \"Between 18 and 65 years\" \"over 65 years\"), :simple false, "
;             ":param \"Count of Participants\", :dispersion nil, :units \"Participants\"}"))))

(def testxml (vtd/navigator (slurp "test/app/testxml3.xml")))

(deftest testxml-4
  (is (not (println (ctgov-import testxml)))))

; (deftest outcome-measurement-properties-test
;   (is (= (outcome-measurement-properties baseline-xml)
;          {
;           :simple false
;           :categories category-info
;           :param param
;           :dispersion dispersion
;           :units units
;           :unit-of-analysis (= 3 measure-count) })))


; (deftest measurement-meta-rdf-test
;   (is (= (measurement-meta-rdf subj "outcome-uri" "group-uri" "mm-uri")
;          [[:uri "http://subject.com"]
;           (list
;            [[:qname :ontology "of_outcome"] [:lit "outcome-uri"]]
;            [[:qname :ontology "of_group"] [:lit "group-uri"]]
;            [[:qname :ontology "of_moment"] [:lit "mm-uri"]])])))

; (deftest baseline-measurement-data-rdf-test
;   (is (= (baseline-measurement-data-rdf subj baseline-xml baseline-xml "group-uri" category-uris)
;          ())))

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
