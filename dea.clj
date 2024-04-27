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
                        (false? (get table key)))
                      (map (fn [c]
                             (hash-set
                               (last (find-transition (first key) c))
                               (last (find-transition (or (second key) (first key)) c))))
                           alphabet))
                  false
                  old))))
          (get-same-states [table]
            (map first (filter (fn [[k v]] (and v (= 2 (count k)))) table)))]
    (-> (perms states)
        init-table
        mark-non-accept
        mark-transition-into-already-marked
        get-same-states)))

(defn merge-states
  [{:keys [states alphabet transitions start accept]} s1 s2]
  (let [sn (str s1 "_" s2)]
    (letfn [(update-transition [[from c to] from' to']
              [(or from' from) c (or to' to)])
            (update-ts-in [transitions]
              (map
                (fn [[from _ to :as t]]
                  (if (contains? #{s1 s2} to)
                    (update-transition t nil sn)
                    t))
                transitions))
            (update-ts-out [transitions]
              (map
                (fn [[from _ to :as t]]
                  (if (contains? #{s1 s2} from)
                    (update-transition t sn nil)
                    t))
                transitions))]
      {:states (conj (disj states s1 s2) sn)
       :alphabet alphabet
       :transitions (apply hash-set (update-ts-out (update-ts-in transitions)))
       :start (if (contains? #{s1 s2} start) sn start)
       :accept (if (empty? (set/intersection accept #{s1 s2}))
                 accept
                 (conj (disj accept s1 s2) sn))})))

(defn simplify-dea
  [dea same-states]
  (reduce (fn [dea states]
            (merge-states dea (first states) (second states)))
          dea
          same-states))


(defn test-simplify
  []
  (map
    (fn [input]
      (let [non-min-res (run-dea dea-not-minimal input)
            min-dea (simplify-dea dea-not-minimal (myhill-nerode dea-not-minimal))
            min-res (run-dea min-dea input)]
        (println "max" non-min-res)
        (println "min" min-res)))
    ["0001" "1" "0" "01" "10" "101" "010" "000111" "111000"]))

(def nea-baa
  {:states #{"q_0" "q_1", "q_2"}
   :alphabet #{\a \b}
   :transitions #{["q_0" \b "q_1"]
                  ["q_1" \a "q_1"]
                  ["q_1" \a "q_2"]
                  ["q_1" \b "q_2"]
                  ["q_2" \a "q_0"]}
   :start "q_0"
   :accept #{"q_0"}})

(def nea-with-epsilon
  {:states (apply hash-set (map #(str "q_" %) (range 0 6)))
   :alphabet #{\0}
   :transitions #{["q_0" 'epsilon "q_1"]
                  ["q_0" 'epsilon "q_3"]
                  ["q_1" \0 "q_2"]
                  ["q_2" \0 "q_1"]
                  ["q_3" \0 "q_4"]
                  ["q_4" \0 "q_5"]
                  ["q_5" \0 "q_3"]}
   :start "q_0"
   :accept #{"q_1" "q_3"}})

(def nea-a-in-3rd-to-last
  {:states (apply hash-set (map #(str "q_" %)) (range 0 4))
   :alphabet #{\a \b}
   :transitions #{["q0" \a "q_1"]
                  ["q0" \a "q_0"]
                  ["q0" \b "q_0"]
                  ["q1" \a "q_2"]
                  ["q1" \b "q_2"]
                  ["q2" \a "q_3"]
                  ["q2" \b "q_3"]}
   :start "q_0"
   :accept #{"q_3"}})

(defn run-nea
  [{:keys [states alphabet transitions start accept]} input]
  (letfn [(find-transitions [c s1]
            (filter (fn [[s1' c' _]]
                      (or
                        (and (= c' 'epsilon) (= s1 s1'))
                        (and (= s1 s1') (= c c'))))
                    transitions))
          (find-next-states [current-states c]
            (apply concat
                   (map (partial find-transitions c) current-states)))
          (step [current-states [c & input]]
            (if (nil? c)
              [current-states (or (some (partial contains? accept) current-states) false)]
              (recur (map last (find-next-states current-states c)) input)))]
    (step (set/union #{start} (find-transitions 'epsilon start)) input)))

; (dea/run-nea dea/nea-baa "baa")
; [("q_0" "q_2" "q_1") true]
;
; (dea/run-nea dea/nea-baa "bab")
; [("q_2") false]

; (dea/run-nea dea/nea-with-epsilon "")
; [("q_1" "q_3") true]
;
; (dea/run-nea dea/nea-with-epsilon "0")
; [("q_2" "q_4") false]
;
; (dea/run-nea dea/nea-with-epsilon "00")
; [("q_1" "q_5") true]
;
; (dea/run-nea dea/nea-with-epsilon "000")
; [("q_2" "q_3") true]
;
; (dea/run-nea dea/nea-with-epsilon "0000")
; [("q_1" "q_4") true]
;
; (dea/run-nea dea/nea-with-epsilon "00000")
; [("q_2" "q_5") false]

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


; @todo: pretty print dea
; @todo: cmd:
;   - parse dea
;   - simplify dea
;   - run dea
;   - print steps
