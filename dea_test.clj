; (use 'clojure.test)
; (run-tests 'dea-test)
(ns dea-test
  (:require [clojure.test :refer [are deftest is testing]]
            [dea]))

(deftest run-dea-drehkreuz-test
  (let [run (partial dea/run-dea dea/dea-drehkreuz)]
    (testing "Drehkreuz accepts only on closed state."
      (are [expected actual] (= expected actual)
           ["V" true] (run "")
           ["V" true] (run "D")
           ["V" true] (run "DDDD")
           ["E" false] (run "DDDDF")
           ["E" false] (run "DDDDFFFFF")
           ["V" true] (run "DDDDFFFFFD")
           ["V" true] (run "DDDDFFFFFDDDD")))))

(deftest nea->dea-test
  (testing "Convert NEA to DEA"
    (is ())))


; @todo: test.check: property-based testing
;        eg., nea->dea should accept for the same inputs as the original nea
