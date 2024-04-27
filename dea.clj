; clj -M -m dea
; repl:
; clj
; > (use '[clojure.tools.namespace.repl :only (refresh)])
; > (use 'dea)
; > (-main)
; make changes
; (refresh)
; useful:
; *e (show exception / stack trace)
(ns dea
  (:require [clojure.set :as set]))

(defn tr
  [s1 r s2]
  (map #(vector s1 (str %) s2) r))

;     | F D
; ---------
; V/E | E V
;  E  | E V
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
                    [["q_0" "-" "m"] ["q_0" "0" "z"]]
                    (tr "p" (range 0 10) "p")
                    [["p" "-" "e"]]
                    (tr "e" alphabet "e")
                    (tr "m" (range 1 10) "p")
                    [["m" "0" "e"] ["m" "-" "e"]]
                    (tr "z" alphabet "e"))
     :start "q_0"
     :accept #{"p" "z"}}))

(def dea-not-minimal
  {:states #{"z_0" "z_1" "z_2" "z_3"}
   :alphabet #{\0 \1}
   :transitions #{["z_0" \0 "z_1"]
                  ["z_0" \1 "z_2"]
                  ["z_1" \0 "z_2"]
                  ["z_1" \1 "z_3"]
                  ["z_2" \0 "z_1"]
                  ["z_2" \1 "z_3"]
                  ["z_3" \0 "z_3"]
                  ["z_3" \1 "z_3"]}
   :start "z_0"
   :accept #{"z_3"}})

(defn run-dea
  [{:keys [states alphabet transitions start accept]} input]
  (letfn [(find-transition [s1 c]
            (first (filter (fn [[s1' c' _]] (and (= (str s1') (str s1)) (= (str c') (str c)))) transitions)))
          (step [state [c & input]]
            (if (nil? c)
              [state (contains? accept state)]
              (recur (last (find-transition state c)) input)))]
    (step start input)))

(defn myhill-nerode
  [{:keys [states alphabet transitions accept] :as dea}]
  (letfn [(perms [xs]
            (apply concat (map (fn [c] (map (fn [c'] [c c']) xs)) xs)))
          (init-table [perms]
            (apply merge (map (fn [[a b]] {(hash-set a b) true}) perms)))
          (reduce-table [table f]
            (reduce
              (fn [table key]
                (update table key (partial f key)))
              table
              (keys table)))
          (find-transition [s1 c]
            (first (filter (fn [[s1' c' _]] (and (= (str s1') (str s1)) (= (str c') (str c)))) transitions)))
          (mark-non-accept [table]
            (reduce-table table
              (fn [key old]
                (if (=
                     (contains? accept (first key))
                     (contains? accept (or (second key) (first key))))
                  old
                  false))))
          (mark-transition-into-already-marked [table]
            (reduce-table table
              (fn [key old]
                (if (some
                      (fn [key]
                        (false? (do (println key) (get table key))))
                      (map (fn [c]
                             (hash-set
                               (last (find-transition (first key) c))
                               (last (find-transition (or (second key) (first key)) c))))
                           alphabet))
                  false
                  old))))]
    (-> (perms states)
        init-table
        mark-non-accept
        mark-transition-into-already-marked)))

; (dea/-main "drehkreuz" "" "D" "DF" "DFFF" "DFFFD")
; ([V true] [V true] [E false] [E false] [V true])
;
; (dea/-main "even" "" "0" "1" "01" "010")
; ([E true] [E true] [O false] [O false] [E true])
;
; (dea/-main "natural-numbers" "" "0" "1" "54-7" "100" "-" "05" "5")
; ([q_0 false] [z true] [p true] [e false] [p true] [m false] [e false] [p true])
(defn -main
  [dea & args]
  (let [deas
        {"even" dea-even
         "drehkreuz" dea-drehkreuz
         "natural-numbers" dea-whole-numbers}]
    (println (map (partial run-dea (get deas dea)) args))))
