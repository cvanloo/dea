; clj -M -m dea
; repl:
; clj
; > (use '[clojure.tools.namespace.repl :only (refresh)])
; > (use 'dea)
; > (-main)
; make changes
; (refresh)
(ns dea)

(def dea-drehkreuz
  {:states [\V \E]
   :alphabet [\F \D]
   :transitions [[\V \F \E]
                 [\V \D \V]
                 [\E \F \E]
                 [\E \D \V]]
   :start \V
   :accept #{\V}})

(defn run-dea
  [{:keys [states alphabet transitions start accept]} input]
  (letfn [(find-transition [s1 c]
            (first (filter (fn [[s1' c' _]] (and (= s1' s1) (= c' c))) transitions)))
          (step [state [c & input]]
            (if (nil? c)
              [state (contains? accept state)]
              (recur (last (find-transition state c)) input)))]
    (step start input)))

; user=> (dea/-main "" "D" "DF" "DFFF" "DFFFD")
; ([V true] [V true] [E false] [E false] [V true])
(defn -main
  [& args]
  (println (map (partial run-dea dea-drehkreuz) args)))
