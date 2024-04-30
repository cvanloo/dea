; clj -M -m dea
; repl:
; clj
; > (use '[clojure.tools.namespace.repl :only (refresh)])
; > (require 'dea)
; make changes
; > (refresh)
; useful:
; > *e ; show exception / stack trace
(ns dea
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn tr
  [s1 r s2]
  (map #(vector s1 (str %) s2) r))

(defn make-dea
 "Tries to create a DEA given a transition table.
  The starting state is the first state from the first transition.
  Accept states are all states written as symbols instead of as strings."
  [transition & transitions]
  (let [transitions (apply hash-set (conj transitions transition))]
    {:states   (set/union
                 (apply hash-set (map (comp str first) transitions))
                 (apply hash-set (map (comp str last) transitions)))
     :alphabet (apply hash-set (map second transitions))
     :transitions (map (fn [[f c t]] [(str f) c (str t)]) transitions)
     :start (str (first transition))
     :accepts (set/union
                (apply hash-set (map str (filter symbol? (map first transitions))))
                (apply hash-set (map str (filter symbol? (map last transitions)))))}))

;     | F D
; ---------
; V/E | E V
;  E  | E V
(def dea-drehkreuz
  (make-dea
    ['V \F \E]
    [\V \D \V]
    [\E \F \E]
    [\E \D \V]))

(def dea-even
  (make-dea
    ['E \0 \E]
    [\E \1 \O]
    [\O \0 \E]
    [\O \1 \O]))

(def dea-whole-numbers
  (let [alphabet (conj (map str (range 0 10)) "-")]
    (into
      (apply make-dea
        (set/union
          (tr "q_0" (range 1 10) "p")
          [["q_0" "-" "m"] ["q_0" "0" "z"]]
          (tr "p" (range 0 10) "p")
          [["p" "-" "e"]]
          (tr "e" alphabet "e")
          (tr "m" (range 1 10) "p")
          [["m" "0" "e"] ["m" "-" "e"]]
          (tr "z" alphabet "e")))
      {:start "q_0"
       :accepts #{"p" "z"}})))

(def dea-not-minimal
  (make-dea
    ["z_0" \0 "z_1"]
    ["z_0" \1 "z_2"]
    ["z_1" \0 "z_2"]
    ["z_1" \1 "z_3"]
    ["z_2" \0 "z_1"]
    ["z_2" \1 "z_3"]
    ["z_3" \0 "z_3"]
    ["z_3" \1 'z_3]))

(defn run-dea
  [{:keys [states alphabet transitions start accepts]} input]
  (letfn [(find-transition [s1 c]
            (first (filter (fn [[s1' c' _]]
                             (and (= (str s1') (str s1))
                                  (= (str c') (str c))))
                           transitions)))
          (step [state [c & input]]
            (if (nil? c)
              [state (contains? accepts state)]
              (recur (last (find-transition state c)) input)))]
    (step start input)))

(defn myhill-nerode
  [{:keys [states alphabet transitions accepts] :as dea}]
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
            (first (filter (fn [[s1' c' _]]
                             (and (= (str s1') (str s1))
                                  (= (str c') (str c))))
                           transitions)))
          (mark-non-accepts [table]
            (reduce-table table
              (fn [key old]
                (if (=
                     (contains? accepts (first key))
            partial          (contains? accepts (or (second key) (first key))))
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
            (map first (filter (fn [[k v]]
                                 (and v (= 2 (count k))))
                               table)))]
    (-> (perms states)
        init-table
        mark-non-accepts
        mark-transition-into-already-marked
        get-same-states)))

(defn merge-states
  [{:keys [states alphabet transitions start accepts]} s1 s2]
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
       :accepts (if (empty? (set/intersection accepts #{s1 s2}))
                 accepts
                 (conj (disj accepts s1 s2) sn))})))

(defn simplify-dea
  [simplify-f dea]
  (reduce (fn [dea states]
            (merge-states dea (first states) (second states)))
          dea
          (simplify-f dea)))

(def simplify-with-myhill-nerode (partial simplify-dea myhill-nerode))

; (dea/simplify-with-myhill-nerode dea/dea-not-minimal)
; {:states #{"z_2_z_1" "z_3" "z_0"}
;  :alphabet #{\0 \1}
;  :transitions #{["z_3" \0 "z_3"]
;                 ["z_0" \0 "z_2_z_1"]
;                 ["z_0" \1 "z_2_z_1"]
;                 ["z_2_z_1" \1 "z_3"]
;                 ["z_2_z_1" \0 "z_2_z_1"]
;                 ["z_3" \1 "z_3"]}
;  :start "z_0"
;  :accepts #{"z_3"}}

; must start with a b
; can contain at most one b in the middle
; after the middle b, only exactly one more a can appear
(def nea-baa
  {:states #{"q_0" "q_1", "q_2"}
   :alphabet #{\a \b}
   :transitions #{["q_0" \b "q_1"]
                  ["q_1" \a "q_1"]
                  ["q_1" \a "q_2"]
                  ["q_1" \b "q_2"]
                  ["q_2" \a "q_0"]}
   :start "q_0"
   :accepts #{"q_0"}})

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
   :accepts #{"q_1" "q_3"}})

(def nea-a-in-3rd-to-last
  {:states (apply hash-set (map #(str "q_" %)) (range 0 4))
   :alphabet #{\a \b}
   :transitions #{["q_0" \a "q_1"]
                  ["q_0" \a "q_0"]
                  ["q_0" \b "q_0"]
                  ["q_1" \a "q_2"]
                  ["q_1" \b "q_2"]
                  ["q_2" \a "q_3"]
                  ["q_2" \b "q_3"]}
   :start "q_0"
   :accepts #{"q_3"}})

(def nea-with-epsilon-2
  {:states #{"q_0" "q_1" "q_2"}
   :alphabet #{\a \b}
   :transitions #{["q_0" \b "q_1"]
                  ["q_1" \a "q_1"]
                  ["q_1" \a "q_2"]
                  ["q_1" \b "q_2"]
                  ["q_2" \a "q_0"]
                  ["q_0" 'epsilon "q_2"]}
   :start "q_0"
   :accepts #{"q_0"}})

(defn run-nea
  [{:keys [states alphabet transitions start accepts] :as nea} input]
  (letfn [(find-transitions [c from]
            (filter (fn [[from' c' _]]
                      (and (= from' from) (= c' c)))
                    transitions))
          (targets [c states]
            (->> states
                 (map (partial find-transitions c))
                 (apply concat)
                 (map last)))
          (take-all-epsilons [states]
            (let [epsilon-states (targets 'epsilon states)]
              (if (empty? epsilon-states)
                []
                (set/union epsilon-states (take-all-epsilons epsilon-states)))))
          (step [current-states [c & input]]
            (if (nil? c)
              [current-states
               (or (some (partial contains? accepts) current-states) false)]
              (let [next-states (targets c current-states)
                    next-states (set/union next-states (take-all-epsilons next-states))]
                (recur (apply hash-set next-states) input))))]
    (step
      (set/union
        #{start}
        (map last (find-transitions 'epsilon start)))
      input)))

; (dea/run-nea dea/nea-with-epsilon "")
; [("q_0" "q_1" "q_3") true]
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
;
; (dea/run-nea dea/nea-with-epsilon "000000")
; [#{"q_1" "q_3"} true]


; user=> (dea/run-nea dea/nea-with-epsilon-2 "")
; [#{"q_2" "q_0"} true]
; user=> (dea/run-nea dea/nea-with-epsilon-2 "b")
; [#{"q_1"} false]
; user=> (dea/run-nea dea/nea-with-epsilon-2 "a")
; [#{"q_2" "q_0"} true]
; user=> (dea/run-nea dea/nea-with-epsilon-2 "baa")
; [#{"q_2" "q_1" "q_0"} true]


; (dea/run-nea dea/nea-a-in-3rd-to-last "")
; [#{"q_0"} false]
;
; (dea/run-nea dea/nea-a-in-3rd-to-last "a")
; [#{"q_1" "q_0"} false]
;
; (dea/run-nea dea/nea-a-in-3rd-to-last "aa")
; [#{"q_2" "q_1" "q_0"} false]
;
; (dea/run-nea dea/nea-a-in-3rd-to-last "aaa")
; [#{"q_2" "q_1" "q_0" "q_3"} true]
;
; (dea/run-nea dea/nea-a-in-3rd-to-last "aaab")
; [#{"q_2" "q_0" "q_3"} true]
;
; (dea/run-nea dea/nea-a-in-3rd-to-last "aaaba")
; [#{"q_1" "q_0" "q_3"} true]
;
; (dea/run-nea dea/nea-a-in-3rd-to-last "aaabaa")
; [#{"q_2" "q_1" "q_0"} false]
;
; (dea/run-nea dea/nea-a-in-3rd-to-last "aaabaab")
; [#{"q_2" "q_0" "q_3"} true]


; (dea/run-nea dea/nea-baa "")
; [#{"q_0"} true]
;
; (dea/run-nea dea/nea-baa "a")
; [#{} false]
;
; (dea/run-nea dea/nea-baa "b")
; [#{"q_1"} false]
;
; (dea/run-nea dea/nea-baa "ba")
; [#{"q_2" "q_1"} false]
;
; (dea/run-nea dea/nea-baa "baa")
; [#{"q_2" "q_1" "q_0"} true]

(defn find-transitions [c from]
  (filter (fn [[from' c' _]]
            (and (= from' from) (= c' c)))
          (:transitions nea-with-epsilon)))

; @todo: make it work with epsilon NEAs as well
(defn nea->dea
  [{:keys [states alphabet transitions start accepts] :as nea}]
  (letfn [(find-transitions [c from]
            (filter (fn [[from' c' _]]
                      (and (= from' from) (= c' c)))
                    transitions))
          (targets [c state]
            (map last (find-transitions c state)))
          (take-all-epsilons [states]
            (let [epsilon-states (targets 'epsilon states)]
              (if (empty? epsilon-states)
                []
                (set/union epsilon-states (take-all-epsilons epsilon-states)))))
          (combine-name [states]
            (if (empty? states)
              "*reject*" ; @fixme: ensure name is not used
              (str/join "-" (apply hash-set states))))
          (map-states [combined-states]
            {(apply hash-set combined-states)
             (reduce into
                     (map (fn [c]
                            {c (apply concat
                                      (map (fn [state]
                                             (let [ss (targets c state)
                                                   ss (set/union ss (take-all-epsilons ss))]
                                               (println "state=" state "c=" c "target=" (targets c state))
                                               ss))
                                           combined-states))})
                          alphabet))})
          (get-missing [m]
            (filter
              (fn [cs]
                (and (not (empty? cs))
                     (not (contains? m (apply hash-set cs))) ))
              (apply concat (map vals (vals m)))))
          (->tr [[cs-from v]]
            (map
              (fn [[c cs-to]]
                [(combine-name cs-from)
                 c
                 (combine-name cs-to)])
              v))
          (m->dea [m]
            {:states (conj (map combine-name (keys m)) "*reject*")
             :alphabet alphabet
             :transitions (set/union
                            (apply set/union (map ->tr m))
                            (tr "*reject*" alphabet "*reject*"))
             :start start
             :accepts (apply hash-set (map combine-name (filter #(not (empty? (set/intersection accepts %))) (keys m))))})]
    (loop [missing [(set/union
                      #{start}
                      (map last (find-transitions 'epsilon start)))]
           m {}]
      (if (empty? missing)
        (m->dea m)
        (let [m's (map map-states missing)
              m'  (reduce into m m's)
              missing' (get-missing m')]
          (recur
            missing'
            m'))))))

; (dea/nea->dea dea/nea-baa)
; {#{"q_0"} {\a (), \b ("q_1")},
;  #{"q_1"} {\a ("q_2" "q_1"), \b ("q_2")},
;  #{"q_2" "q_1"} {\a ("q_0" "q_2" "q_1"), \b ("q_2")},
;  #{"q_2"} {\a ("q_0"), \b ()},
;  #{"q_2" "q_1" "q_0"} {\a ("q_0" "q_2" "q_1"), \b ("q_1" "q_2")}}

; (dea/m->dea (dea/nea->dea dea/nea-baa))
; {:states ("*reject*" "q_0" "q_1" "q_2-q_1" "q_2" "q_2-q_1-q_0"),
;  :alphabet #{\a \b},
;  :transitions (["*reject*" "b" "*reject*"]
;                ["*reject*" "a" "*reject*"]
;                ["q_2" \b "*reject*"]
;                ["q_2" \a "q_0"]
;                ["q_2-q_1" \b "q_2"]
;                ["q_2-q_1" \a "q_2-q_1-q_0"]
;                ["q_1" \b "q_2"]
;                ["q_1" \a "q_2-q_1"]
;                ["q_0" \b "q_1"]
;                ["q_0" \a "*reject*"]
;                ["q_2-q_1-q_0" \a "q_2-q_1-q_0"]
;                ["q_2-q_1-q_0" \b "q_2-q_1"]),
;  :start "q_0",
;  :accepts #{"q_2-q_1-q_0" "q_0"}}


; {:states ("*reject*" "q_2-q_0" "q_0" "q_1" "q_2-q_1" "q_2" "q_2-q_1-q_0"),
;  :alphabet #{\a \b},
;  :transitions (["*reject*" "b" "*reject*"]
;                ["*reject*" "a" "*reject*"]
;                ["q_2" \b "*reject*"]
;                ["q_2" \a "q_0"]
;                ["q_2-q_1" \b "q_2"]
;                ["q_2-q_1" \a "q_2-q_1-q_0"]
;                ["q_1" \b "q_2"]
;                ["q_1" \a "q_2-q_1"]
;                ["q_0" \b "q_1"]
;                ["q_0" \a "*reject*"]
;                ["q_2-q_0" \b "q_1"]
;                ["q_2-q_0" \a "q_0"]
;                ["q_2-q_1-q_0" \a "q_2-q_1-q_0"]
;                ["q_2-q_1-q_0" \b "q_2-q_1"]),
;  :start "q_0",
;  :accepts #{"q_2-q_1-q_0" "q_0" "q_2-q_0"}}

; user=> (dea/run-dea (dea/nea->dea dea/nea-baa) "")
; ["q_0" true]
; user=> (dea/run-dea (dea/nea->dea dea/nea-baa) "a")
; ["*reject*" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-baa) "ba")
; ["q_2-q_1" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-baa) "b")
; ["q_1" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-baa) "ba")
; ["q_2-q_1" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-baa) "baa")
; ["q_2-q_1-q_0" true]

; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "")
; ["q_0" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "a")
; ["q_1-q_0" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "a")
; ["q_1-q_0" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "aa")
; ["q_2-q_1-q_0" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "aaa")
; ["q_2-q_1-q_0-q_3" true]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "aaab")
; ["q_2-q_0-q_3" true]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "aaaba")
; ["q_1-q_0-q_3" true]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "aaabab")
; ["q_2-q_0" false]
; user=> (dea/run-dea (dea/nea->dea dea/nea-a-in-3rd-to-last) "aaababa")
; ["q_1-q_0-q_3" true]


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


; @todo: nea->dea (with nea_epsilon)
; @todo: compare two neas
; @todo: Mengenoperationen (create/combine new nea from other neas)

; @todo: pretty print dea
; @todo: regex to nea (to dea)
; @todo: dea to regex
; @todo: cmd:
;   - parse dea
;   - simplify dea
;   - run dea
;   - print steps
