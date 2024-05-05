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

(deftest run-nea-baa-test
  (let [run (partial dea/run-nea dea/nea-baa)]
    (testing "BAA accepts words starting with 'b', any number of 'a's, and ending with two 'aa's or a 'ba'. Or the empty word."
      (are [expected actual] (= expected actual)
           [#{"q_0"} true] (run "")
           [#{} false] (run "a")
           [#{} false] (run "aa")
           [#{} false] (run "aab")
           [#{} false] (run "aaabaa")
           [#{"q_1"} false] (run "b")
           [#{"q_2"} false] (run "bb")
           [#{} false] (run "bbb")
           [#{} false] (run "bbb")
           [#{"q_0"} true] (run "bba")
           [#{"q_2" "q_1"} false] (run "ba")
           [#{"q_2" "q_1" "q_0"} true] (run "baa")
           [#{"q_2" "q_1" "q_0"} true] (run "baaa")
           [#{"q_2" "q_1"} false] (run "baaab")
           [#{"q_2" "q_1" "q_0"} true] (run "baaaba")
           [#{"q_2" "q_1" "q_0"} true] (run "baaabaa")
           [#{"q_2" "q_1"} false] (run "baaabaab")))))

(deftest run-nea-with-epsilon-test
  (let [run (partial dea/run-nea dea/nea-with-epsilon)]
    (testing "With Epsilon accepts word where |word| is divisible by 2 or 3."
      (are [expected actual] (= expected actual)
           [#{"q_0" "q_1" "q_3"} true] (run "")
           [#{"q_2" "q_4"} false] (run "0")
           [#{"q_1" "q_5"} true] (run "00")
           [#{"q_2" "q_3"} true] (run "000")
           [#{"q_1" "q_4"} true] (run "0000")
           [#{"q_2" "q_5"} false] (run "00000")
           [#{"q_1" "q_3"} true] (run "000000")))))

(deftest run-nea-a-in-3rd-to-last-test
  (let [run (partial dea/run-nea dea/nea-a-in-3rd-to-last)]
    (testing "A in 3rd to last accepts inputs where an a is in the 3rd to last position."
      (are [expected actual] (= expected actual)
           [#{"q_0"} false] (run "")
           [#{"q_0"} false] (run "b")
           [#{"q_0" "q_1"} false] (run "a")
           [#{"q_0" "q_1" "q_2"} false] (run "aa")
           [#{"q_0" "q_2"} false] (run "ab")
           [#{"q_0" "q_1" "q_2" "q_3"} true] (run "aaa")
           [#{"q_0" "q_1" "q_3"} true] (run "aba")
           [#{"q_2" "q_0" "q_3"} true] (run "aaab")
           [#{"q_2" "q_1" "q_0" "q_3"} true] (run "aaaa")
           [#{"q_2" "q_0"} false] (run "aaaabab")
           [#{"q_0" "q_3"} true] (run "aaaababb")))))

(deftest run-nea-with-epsilon-2-test
  (let [run (partial dea/run-nea dea/nea-with-epsilon-2)]
    (testing "Similar to nea-baa, but also accepts any number of leading and trailing 'a's."
      (are [expected actual] (= expected actual)
           [#{"q_2" "q_0"} true] (run "")
           [#{"q_2" "q_0"} true] (run "a")
           [#{"q_2" "q_0"} true] (run "aa")
           [#{"q_2" "q_0"} true] (run "aaa")
           [#{"q_1"} false] (run "aaab")
           [#{"q_2" "q_0"} true] (run "aaabba")
           [#{"q_2" "q_1" "q_0"} true] (run "aaabaa")
           [#{"q_2" "q_1" "q_0"} true] (run "aaabaaa")
           [#{"q_2" "q_1" "q_0"} true] (run "aaabaaaa")
           [#{"q_2" "q_1"} false] (run "aaabaaaab")
           [#{"q_2" "q_1" "q_0"} true] (run "aaabaaaaba")))))

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
