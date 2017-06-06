(ns app.core-test
  (:require [riveted.core :as vtd])
  (:use clojure.test)
  (:use app.core))


(deftest baseline-measurements-test
  (is
    (= (let
         [xml (vtd/navigator "<measure>
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
          idx 1
          sample-size-xml '()
          baseline-uris (list "baseline-uri")
          group-uris (list "group-uri")
          mm-uris (list "mm-uri")
          category-uris (list "category-uri")]
         (baseline-measurements xml idx sample-size-xml baseline-uris group-uris mm-uris category-uris))

                                      false)))
