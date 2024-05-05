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
  (map #(vector s1 (first (str %)) s2) r))

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

(defn is-dea?
  [{:keys [alphabet states transitions] :as nea-or-dea}]
  (letfn [(has-exactly-one-transition? [from c]
            (= 1 (count (filter (fn [[from' c' _]]
                                  (and (= from' from) (= c' c)))
                                transitions))))
          (map-2 [xs ys]
            (mapcat
              (fn [x]
                (map
                  (fn [y] [x y])
                  ys))
              xs))]
    (and
      (empty? (filter (fn [[_ c _]] (= c 'epsilon)) transitions))
      (every? (partial apply has-exactly-one-transition?)
              (map-2 states alphabet)))))

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
          [["q_0" \- "m"] ["q_0" \0 "z"]]
          (tr "p" (range 0 10) "p")
          [["p" \- "e"]]
          (tr "e" alphabet "e")
          (tr "m" (range 1 10) "p")
          [["m" \0 "e"] ["m" \- "e"]]
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
                     (contains? accepts (or (second key) (first key))))
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

(def is-minimal? (comp empty? myhill-nerode))

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

(defn remove-unreachable-states
  [{:keys [alphabet transitions start accepts] :as dea}]
  (letfn [(transition-targets [s]
            (map last (filter
                        #(= s (first %))
                        transitions)))]
    (loop [[s & ss] [start]
           states #{}]
      (if (nil? s)
        {:states states
         :alphabet alphabet
         :transitions (filter #(contains? states (first %)) transitions)
         :start start
         :accepts (set/intersection accepts states)}
        (recur
          (concat ss (filter
                       #(not (contains? states %))
                       (transition-targets s)))
          (conj states s))))))

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
              [(apply hash-set current-states)
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

(defn unique-name
  ([states]
   (unique-name states "s"))
  ([states name]
   (if (contains? states name)
     (recur states (str name "'"))
     name)))

(defn nea->dea
  [{:keys [states alphabet transitions start accepts] :as nea}]
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
          (combine-name [states]
            (if (empty? states)
              "*reject*" ; @fixme: ensure name is not used
              (str/join "-" (apply hash-set states))))
          (map-states [combined-states]
            {(apply hash-set combined-states)
             (reduce into
                     (map (fn [c]
                            {c (let [ss (targets c combined-states)
                                     ss (set/union ss (take-all-epsilons ss))]
                                 (apply hash-set ss))})
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
                            (tr "*reject*" alphabet "*reject*")) ; @todo: don't add reject state unles it is actually reachable
             :start (combine-name (set/union
                                    #{start}
                                    (map last (find-transitions 'epsilon start))))
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

; (dea/nea->dea dea/nea-with-epsilon-2)
; {:states ("*reject*" "q_2-q_0" "q_1" "q_2-q_1" "q_2" "q_2-q_1-q_0"),
;  :alphabet #{\a \b},
;  :transitions (["*reject*" "b" "*reject*"]
;                ["*reject*" "a" "*reject*"]
;                ["q_2" \b "*reject*"]
;                ["q_2" \a "q_2-q_0"]
;                ["q_2-q_1" \b "q_2"]
;                ["q_2-q_1" \a "q_2-q_1-q_0"]
;                ["q_1" \b "q_2"]
;                ["q_1" \a "q_2-q_1"]
;                ["q_2-q_0" \b "q_1"]
;                ["q_2-q_0" \a "q_2-q_0"]
;                ["q_2-q_1-q_0" \a "q_2-q_1-q_0"]
;                ["q_2-q_1-q_0" \b "q_2-q_1"]),
;  :start "q_2-q_0",
;  :accepts #{"q_2-q_1-q_0" "q_2-q_0"}}


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

; (pprint (dea/nea->dea dea/dea-drehkreuz))
; {:states ("*reject*" "V" "E"),
;  :alphabet #{\D \F},
;  :transitions
;  (["*reject*" "F" "*reject*"]
;   ["*reject*" "D" "*reject*"]
;   ["E" \F "E"]
;   ["E" \D "V"]
;   ["V" \D "V"]
;   ["V" \F "E"]),
;  :start "V",
;  :accepts #{"V"}}

; (pprint (dea/remove-unreachable-states (dea/nea->dea dea/dea-drehkreuz)))
; {:states #{"E" "V"},
;  :alphabet #{\D \F},
;  :transitions (["E" \F "E"] ["E" \D "V"] ["V" \D "V"] ["V" \F "E"]),
;  :start "V",
;  :accepts #{"V"}}


(def dea-eq
  {:states #{"z_0" "z_1", "z_2"}
   :alphabet #{\a \b}
   :transitions #{["z_0" \a "z_1"]
                  ["z_0" \b "z_2"]
                  ["z_1" \a "z_1"]
                  ["z_1" \b "z_2"]
                  ["z_2" \a "z_0"]
                  ["z_2" \b "z_2"]}
   :start "z_0"
   :accepts #{"z_0"}})

(def dea-eq'
  {:states #{"q_0" "q_1", "q_2"}
   :alphabet #{\a \b}
   :transitions #{["q_0" \a "q_1"]
                  ["q_0" \b "q_2"]
                  ["q_1" \a "q_1"]
                  ["q_1" \b "q_2"]
                  ["q_2" \a "q_2"]
                  ["q_2" \b "q_2"]}
   :start "q_0"
   :accepts #{"q_0"}})

(defn nea-eq?
  [nea-1 nea-2]
  (defn follow-states
    [alphabet transitions1 transitions2 s1 s2]
    (letfn [(target [transitions c from]
              (last (first (filter
                             (fn [[from' c' _]]
                               (and (= c c')
                                    (= from from')))
                             transitions))))
            (targets [transitions alphabet from]
              (map
                (fn [c]
                  (target transitions c from))
                alphabet))
            ; @todo: check that both states are of the same type (accept/not accept)
            (compare-targets [m t1 t2]
              (let [r (get m t1)]
                (cond
                  (nil? r) [:states-new t1 t2]
                  (= r t2) [:states-equal]
                  :else [:states-not-equal])))]
      (loop [[s1 & ss1] [s1]
             [s2 & ss2] [s2]
             m {s1 s2}]
        (let [t1s (targets transitions1 alphabet s1)
              t2s (targets transitions2 alphabet s2)
              eqs? (map #(compare-targets m %1 %2) t1s t2s)
              news (filter #(= :states-new (first %)) eqs?)]
          ;(clojure.pprint/pprint {"s1" s1 "s2" s2 "m" m "t1s" t1s "t2s" t2s "eqs?" eqs? "news" news})
          (cond
            (some #(= :states-not-equal (first %)) eqs?)
            false
            (not (and (empty? ss1) (empty? news)))
            (recur
              (concat ss1 (map #(nth % 1) news))
              (concat ss2 (map #(nth % 2) news))
              (reduce (fn [m [_ t1 t2]]
                        (into m {t1 t2}))
                      m
                      news))
            :else true)))))
  (let [->dea (comp simplify-with-myhill-nerode remove-unreachable-states nea->dea)
        dea-1 (->dea nea-1)
        dea-2 (->dea nea-2)]
    (and
      (apply = (map :alphabet [dea-1 dea-2]))
      (apply = (map (comp count :states) [dea-1 dea-2]))
      (apply = (map (comp count :accepts) [dea-1 dea-2]))
      (follow-states
        (:alphabet dea-1)
        (:transitions dea-1)
        (:transitions dea-2)
        (:start dea-1)
        (:start dea-2)))))

; user=> (dea/nea-eq? dea/dea-eq dea/dea-eq')
; false
; user=> (dea/nea-eq? dea/dea-eq dea/dea-eq)
; true
; user=> (dea/nea-eq? dea/dea-not-minimal (dea/simplify-with-myhill-nerode dea/dea-not-minimal))
; true

(def dea-l-div-by-2
  (make-dea
    ['q_1 \0 "q_2"]
    ["q_2" \0 "q_1"]))

(def dea-l-div-by-3
  (make-dea
    ['q_3 \0 "q_4"]
    ["q_4" \0 "q_5"]
    ["q_5" \0 "q_3"]))

(def nea-3-as
  (make-dea
    ["q_0" \a "q_1"]
    ["q_1" \a "q_2"]
    ["q_2" \a 'q_3]))

(def nea-2-bs
  (make-dea
    ["z_0" \b "z_1"]
    ["z_1" \b 'z_2]))

(defn alternative
  [nea-1 nea-2]
  (let [[states-1 states-2] (map :states [nea-1 nea-2])
        states-2 (map (partial unique-name states-1) states-2)
        states (apply hash-set (set/union states-1 states-2))
        new-start (unique-name states)]
    {:states (conj states new-start)
     :alphabet (apply set/union (map :alphabet [nea-1 nea-2]))
     ; @fixme: transitions also need to be updated with the unique-names
     :transitions (conj (apply set/union (map :transitions [nea-1 nea-2]))
                        [new-start 'epsilon (:start nea-1)]
                        [new-start 'epsilon (:start nea-2)])
     :start new-start
     :accepts (apply set/union (map :accepts [nea-1 nea-2]))}))

(defn product
  [nea-1 nea-2])
; @todo: haven't got a clue
;   do we even need that?

(defn complement
  [nea]
  (let [->dea (comp simplify-with-myhill-nerode remove-unreachable-states nea->dea)
        dea (->dea nea)]
    (into dea
          {:accepts (set/difference (:states dea) (:accepts dea))})))

(defn chain
  [nea-1 nea-2]
  (let [[states-1 states-2] (map :states [nea-1 nea-2])
        states-2 (map (partial unique-name states-1) states-2)
        states (apply hash-set (set/union states-1 states-2))]
    {:states states
     :alphabet (apply set/union (map :alphabet [nea-1 nea-2]))
     ; @fixme: transitions also need to be updated with the unique-names
     :transitions (concat (apply set/union (map :transitions [nea-1 nea-2]))
                          (map (fn [s]
                                 [s 'epsilon (:start nea-2)])
                               (:accepts nea-1)))
     :start (:start nea-1)
     :accepts (:accepts nea-2)}))

(defn *
  [{:keys [states transitions start accepts] :as nea}]
  (let [new-start (unique-name states)]
    {:states (conj states new-start)
     :alphabet (:alphabet nea)
     :transitions (concat
                    transitions
                    [[new-start 'epsilon start]]
                    (map #(vector % 'epsilon new-start) accepts))
     :start new-start
     :accepts #{new-start}}))

;(def + #(chain % (* %)))

(defn +
  [{:keys [states transitions start accepts] :as nea}]
  (let [new-start (unique-name states)]
    {:states (conj states new-start)
     :alphabet (:alphabet nea)
     :transitions (concat
                    transitions
                    [[new-start 'epsilon start]]
                    (map #(vector % 'epsilon new-start) accepts))
     :start new-start
     :accepts accepts}))


; user=> (pprint (dea/alternative dea/dea-l-div-by-2 dea/dea-l-div-by-3))
; {:states #{"s" "q_2" "q_1" "q_3" "q_4" "q_5"},
;  :alphabet #{\0},
;  :transitions
;  (["s" epsilon "q_3"]
;   ["s" epsilon "q_1"]
;   ["q_1" \0 "q_2"]
;   ["q_2" \0 "q_1"]
;   ["q_3" \0 "q_4"]
;   ["q_4" \0 "q_5"]
;   ["q_5" \0 "q_3"]),
;  :start "s",
;  :accepts #{"q_1" "q_3"}}
;
; user=> (pprint (dea/alternative (dea/make-nea-for-char \a) (dea/make-nea-for-char \b)))
; {:states #{"s" "q_0'" "q_1'" "q_1" "q_0"},
;  :alphabet #{\a \b},
;  :transitions #{["s" epsilon "q_0"] ["q_0" \a "q_1"] ["q_0" \b "q_1"]},
;  :start "s",
;  :accepts #{"q_1"}}
;
; user=> (dea/nea-eq? (dea/alternative dea/dea-l-div-by-2 dea/dea-l-div-by-3) dea/nea-with-epsilon)
; true
;
; user=> (dea/run-dea (dea/complement dea/dea-even) "10")
; ["E" false]
; user=> (dea/run-dea (dea/complement dea/dea-even) "11")
; ["O" true]
;
; user=> (dea/run-nea (dea/chain dea/nea-3-as dea/nea-2-bs) "aaa")
; [#{"q_3" "z_0"} false]
; user=> (dea/run-nea (dea/chain dea/nea-3-as dea/nea-2-bs) "aaab")
; [#{"z_1"} false]
; user=> (dea/run-nea (dea/chain dea/nea-3-as dea/nea-2-bs) "aaabb")
; [#{"z_2"} true]
; user=> (dea/run-nea (dea/chain dea/nea-3-as dea/nea-2-bs) "aaabbb")
; [#{} false]
;
; user=> (dea/run-nea (dea/* dea/nea-3-as) "")
; [#{"s" "q_0"} true]
; user=> (dea/run-nea (dea/* dea/nea-3-as) "a")
; [#{"q_1"} false]
; user=> (dea/run-nea (dea/* dea/nea-3-as) "aa")
; [#{"q_2"} false]
; user=> (dea/run-nea (dea/* dea/nea-3-as) "aaa")
; [#{"s" "q_0" "q_3"} true]
; user=> (dea/run-nea (dea/* dea/nea-3-as) "aaaa")
; [#{"q_1"} false]
; user=> (dea/run-nea (dea/* dea/nea-3-as) "aaaaaa")
; [#{"s" "q_0" "q_3"} true]
;
; user=> (dea/run-nea (dea/+ dea/nea-3-as) "")
; [#{"q_0"} false]
; user=> (dea/run-nea (dea/+ dea/nea-3-as) "aa")
; [#{"q_2"} false]
; user=> (dea/run-nea (dea/+ dea/nea-3-as) "aaa")
; [#{"s" "q_0" "q_3"} true]
; user=> (dea/run-nea (dea/+ dea/nea-3-as) "aaaa")
; [#{"q_1"} false]
; user=> (dea/run-nea (dea/+ dea/nea-3-as) "aaaaa")
; [#{"q_2"} false]
; user=> (dea/run-nea (dea/+ dea/nea-3-as) "aaaaaa")
; [#{"s" "q_0" "q_3"} true]

; @todo: single step?
(defn nea->vnea
  [nea])

(defn nea->regex
  [nea])


(defn make-nea-for-char
  [c]
  {:states #{"q_0" "q_1"}
   :alphabet #{c}
   :transitions #{["q_0" c "q_1"]}
   :start "q_0"
   :accepts #{"q_1"}})

(defn make-nea-for-chars
  [cs]
  (let [alphabet (apply hash-set cs)]
    {:states #{"q_0" "q_1"}
     :alphabet alphabet
     :transitions (tr "q_0" alphabet "q_1")
     :start "q_0"
     :accepts #{"q_1"}}))

; r = ab|cd
; (alternative
;   (chain
;     (make-nea-for-char \a)
;     (make-nea-for-char \b))
;   (chain
;     (make-nea-for-char \c)
;     (make-nea-for-char \d)))

; r = (ab)*c
; (chain
;   (* (chain
;        (make-nea-for-char \a)
;        (make-nea-for-char \b)))
;   (make-nea-for-char \c))

; r = (ab)+
; (+ (chain
;      (make-nea-for-char \a)
;      (make-nea-for-char \b)))

(defn regex->nea
  [regex])

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
; @todo: regex to nea (to dea)
; @todo: dea to regex
; @todo: cmd:
;   - parse dea
;   - simplify dea
;   - run dea
;   - print steps
; @todo: clojure.spec? type nea, dea
