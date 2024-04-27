; clj -M -m dea
; repl:
; clj
; > (use '[clojure.tools.namespace.repl :only (refresh)])
; > (use 'dea)
; > (-main)
; make changes
; (refresh)
(ns dea
  (:require [clojure.set :as set]))

(defn tr
  [s1 r s2]
  (map #(vector s1 (str %) s2) r))

(def dea-drehkreuz
  {:states #{\V \E}
   :alphabet #{\F \D}
   :transitions #{[\V \F \E]
                  [\V \D \V]
                  [\E \F \E]
                  [\E \D \V]}
   :start \V
   :accept #{\V}})

(def dea-even
  {:states #{\E \O}
   :alphabet #{\0 \1}
   :transitions #{[\E \0 \E]
                  [\E \1 \O]
                  [\O \0 \E]
                  [\O \1 \O]}
   :start \E
   :accept #{\E}})

(def dea-whole-numbers
  (let [alphabet (apply hash-set (conj (map str (range 0 10)) "-"))]
    {:states #{"q_0" "z" "p" "e" "m"}
     :alphabet alphabet
     :transitions (set/union
                    (tr "q_0" (range 1 10) "p")
                    [["q_0" "-" "m"] ["q_0" "0" "e"]]
                    (tr "p" (range 0 10) "p")
                    [["p" "-" "e"]]
                    (tr "e" alphabet "e")
                    (tr "m" (range 1 10) "p")
                    [["m" "0" "e"] ["m" "-" "e"]])
     :start "q_0"
     :accept #{"p"}}))

(defn run-dea
  [{:keys [states alphabet transitions start accept]} input]
  (letfn [(find-transition [s1 c]
            (first (filter (fn [[s1' c' _]] (and (= (str s1') (str s1)) (= (str c') (str c)))) transitions)))
          (step [state [c & input]]
            (if (nil? c)
              [state (contains? accept state)]
              (recur (last (find-transition state c)) input)))]
    (step start input)))

; (dea/-main "drehkreuz" "" "D" "DF" "DFFF" "DFFFD")
; ([V true] [V true] [E false] [E false] [V true])
;
; (dea/-main "even" "" "0" "1" "01" "010")
; ([E true] [E true] [O false] [O false] [E true])
;
; (dea/-main "natural-numbers" "" "0" "1" "54-7" "100" "-")
; ([q_0 false] [e false] [p true] [e false] [p true] [m false])
(defn -main
  [dea & args]
  (let [deas
        {"even" dea-even
         "drehkreuz" dea-drehkreuz
         "natural-numbers" dea-whole-numbers}]
    (println (map (partial run-dea (get deas dea)) args))))
