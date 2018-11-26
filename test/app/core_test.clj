(ns app.core-test
  (:use clojure.test)
  (:use app.core))

(deftest test-main-not-nil 
  (is (not (nil? -main))))
