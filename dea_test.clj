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

; @todo: test neas / run-nea
;        - nea-baa
;        - nea-with-epsilon
;        - nea-a-in-3rd-to-last
;        - nea-with-epsilon-2

; (deftest is-minimal?-test)
; @todo: prop test: (is (dea/is-minimal? (dea/simplify-with-myhill-nerode some-possibly-not-minimal-dea)))

; (deftest simplify-with-myhill-nerode-test)
; @todo: prop test: same as above?

(deftest nea->dea-test
  (testing "Convert NEA to DEA"
    (is ())))
; @todo: test.check: property-based testing
;        eg., nea->dea should accept for the same inputs as the original nea

; @todo: test remove-unreachable-states

; @todo: test nea-eq?

; @todo: test alternative, product, complement, chain, *, +
