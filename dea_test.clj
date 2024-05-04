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

(deftest run-dea-even-test
  (let [run (partial dea/run-dea dea/dea-even)]
    (testing "Even accepts only even numbers"
      (are [expected actual] (= expected actual)
           ["E" true] (run "")
           ["E" true] (run "0")
           ["O" false] (run "1")
           ["E" true] (run "00")
           ["O" false] (run "01")
           ["E" true] (run "10")
           ["O" false] (run "11")))))

(deftest run-dea-whole-numbers-test
  (let [run (partial dea/run-dea dea/dea-whole-numbers)]
    (testing "Whole Numbers accepts only whole numbers that must not start with a leading zero."
      (are [expected actual] (= expected actual)
           ["q_0" false] (run "")
           ["z" true] (run "0")
           ["e" false] (run "-0")
           ["e" false] (run "-01")
           ["p" true] (run "-10")
           ["p" true] (run "1")
           ["p" true] (run "15")
           ["p" true] (run "105")
           ["p" true] (run "1050")
           ["e" false] (run "1-5")
           ["m" false] (run "-")))))

(deftest nea->dea-test
  (testing "Convert NEA to DEA"
    (is ())))


; @todo: test.check: property-based testing
;        eg., nea->dea should accept for the same inputs as the original nea
