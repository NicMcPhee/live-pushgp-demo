{:title "Interactive PushGP demo"
 :layout :page
 :page-index 2
 :navbar? false
 :klipse true}

## An interactive PushGP demo

### Setup and utilities

```klipse-cljs
(ns propel.core
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [clojure.string :as string]
    [cljs.core.async :as async]
    [reagent.core :as r]))

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :input {}})

; TODO add collapsable components so users can open/close sections
; as they are/aren't interested.
```

```klipse-cljs
(defn abs
  "Absolute value."
  [x]
  (if (neg? x)
    (- x)
    x))
```

### Stack manipulation

```klipse-cljs
(defn push-to-stack
  "Pushes item onto stack in state"
  [state stack item]
  (update state stack conj item))

(defn pop-stack
  "Removes top item of stack."
  [state stack]
  (update state stack rest))

(defn peek-stack
  "Returns top item on a stack."
  [state stack]
  (if (empty? (get state stack))
    :no-stack-item
    (first (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty."
  [state stack]
  (empty? (get state stack)))

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are
  enough args on each of the desired stacks, returns a map of the form
  {:state :args}, where :state is the new state and :args is a list of
  args from the stacks. If there aren't enough args on the stacks,
  returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

; TODO Delete this, as I think it is subsumed by the `multi-result` version
; below.

(defn single-result-push-instruction
  "A utility function for making Push instructions. Takes a state,
  the function to apply to the args, the stacks to take the args from,
  and the stack to return the result to. Applies the function to the
  args (taken from the stacks) and pushes the return value
  onto return-stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

(defn multi-result-push-instruction
  "A utility function for making Push instructions. Takes a state,
  the function to apply to the args, the stacks to take the args
  from, and the stack to return the result(s) to. Applies the
  function to the args (in the order taken from the stacks) and
  produces a list of results, all of which are pushed (in order)
  onto the return stack."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result-seq (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (if (seq? result-seq)
          (update new-state return-stack into (reverse result-seq))
          (push-to-stack new-state return-stack result-seq))))))
```

#### Instructions

```klipse-cljs
(def opens ; number of blocks opened by instructions (default = 0)
  {'exec_dup 1
   'exec_if 2})

(defn in1
  "Pushes the input labeled :in1 from the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))
```

#### Integer instructions

```klipse-cljs
(defn integer_inc
  [state]
  (single-result-push-instruction
    state inc [:integer] :integer))

(defn integer_dec
  [state]
  (single-result-push-instruction
    state dec [:integer] :integer))

(defn integer_+
  [state]
  (single-result-push-instruction
    state + [:integer :integer] :integer))

(defn integer_-
  [state]
  (single-result-push-instruction
    state - [:integer :integer] :integer))

(defn integer_*
  [state]
  (single-result-push-instruction
    state * [:integer :integer] :integer))

(defn integer_%
  [state]
  (single-result-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_=
  [state]
  (single-result-push-instruction state = [:integer :integer] :boolean))

(defn integer_dup
  [state]
  (multi-result-push-instruction
    state #(list % %) [:integer] :integer))

(defn integer_swap
  [state]
  (multi-result-push-instruction
    state #(list %1 %2) [:integer :integer] :integer))
```

#### Exec stack instructions

```klipse-cljs
(defn exec_dup
  [state]
  (multi-result-push-instruction
    state #(list % %) [:exec] :exec))

(defn exec_swap
  [state]
  (multi-result-push-instruction
    state #(list %1 %2) [:exec :exec] :exec))

(defn exec_if
  [state]
  (single-result-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))
```

#### Boolean Instructions

```klipse-cljs
(defn boolean_and
  [state]
  (single-result-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (single-result-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (single-result-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (single-result-push-instruction state = [:boolean :boolean] :boolean))

(defn boolean_dup
  [state]
  (multi-result-push-instruction
    state #(list % %) [:boolean] :boolean))

(defn boolean_swap
  [state]
  (multi-result-push-instruction
    state #(list %1 %2) [:boolean :boolean] :boolean))
```

#### String instructions

```klipse-cljs
(defn string_=
  [state]
  (single-result-push-instruction state = [:string :string] :boolean))

(defn string_dup
  [state]
  (multi-result-push-instruction
    state #(list % %) [:string] :string))

(defn string_swap
  [state]
  (multi-result-push-instruction
    state #(list %1 %2) [:string :string] :string))

(defn string_take
  [state]
  (single-result-push-instruction state
                         #(apply str (take %1 %2))
                         [:integer :string]
                         :string))

(defn string_drop
  [state]
  (single-result-push-instruction state
                         #(apply str (drop %1 %2))
                         [:integer :string]
                         :string))

(defn string_reverse
  [state]
  (single-result-push-instruction state
                         #(apply str (reverse %))
                         [:string]
                         :string))

(defn string_concat
  [state]
  (single-result-push-instruction state
                         #(apply str (concat %1 %2))
                         [:string :string]
                         :string))

(defn string_length
  [state]
  (single-result-push-instruction state count [:string] :integer))

(defn string_includes?
  [state]
  (single-result-push-instruction state clojure.string/includes? [:string :string] :boolean))
```

#### Bringing all the instructions together

```klipse-cljs
; TODO Split instructions into separate groups (e.g., integers, booleans, etc.)
; so we can more easily control what gets included and what doesn't when we
; do runs.

(def push-instruction-registry
  {'in1 in1
   'integer_inc integer_inc
   'integer_dec integer_dec
   'integer_+ integer_+
   'integer_- integer_-
   'integer_* integer_*
   'integer_% integer_%
   'integer_= integer_=
   'integer_dup integer_dup
   'integer_swap integer_swap
   'exec_dup exec_dup
   'exec_swap exec_swap
   'exec_if exec_if
   'boolean_and boolean_and
   'boolean_or boolean_or
   'boolean_not boolean_not
   'boolean_= boolean_=
   'boolean_dup boolean_dup
   'boolean_swap boolean_swap
   'string_= string_=
   'string_dup string_dup
   'string_swap string_swap
   'string_take string_take
   'string_drop string_drop
   'string_reverse string_reverse
   'string_concat string_concat
   'string_length string_length
   'string_includes? string_includes?
   })

(def default-instructions
  (concat (keys push-instruction-registry)
    ['close
     0
     1
     10
     100
     1000
     10000
     true
     false
     ""
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     "A"
     "C"
     "G"
     "T"
     ]))
```

## Push interpreter

```klipse-cljs
(defn cljc-throw
  [item]
  (throw
    (js/Error.
      (str
        "Unrecognized Push instruction in program: "
        item))))

(defn push-instruction?
  [item]
  (contains? push-instruction-registry item))

(defn interpret-one-step
  "Takes a Push state and executes the next instruction on the exec stack."
  [state]
  (let [popped-state (pop-stack state :exec)
        first-instruction (first (:exec state))]
    (cond
      (push-instruction? first-instruction)
        ((get push-instruction-registry first-instruction) popped-state)
      (integer? first-instruction)
        (push-to-stack popped-state :integer first-instruction)
      (string? first-instruction)
        (push-to-stack popped-state :string first-instruction)
      (seq? first-instruction)
        (update popped-state :exec #(concat %2 %1) first-instruction)
      (or (= first-instruction true) (= first-instruction false))
        (push-to-stack popped-state :boolean first-instruction)
      :else
        (cljc-throw first-instruction)
      )))

(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))

(defn push-from-plushy
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opens
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get opens %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else
```

## PushGP

### Parent selection

```klipse-cljs
(defn tournament-selection
  "Selects an individual from the population using a tournament."
  [pop argmap]
  (let [tournament-size (:tournament-size argmap)
        tournament-set (take tournament-size (shuffle pop))]
    (apply min-key :total-error tournament-set)))

(defn lexicase-selection
  "Selects an individual from the population using lexicase selection."
  [pop argmap]
  (loop [survivors pop
         cases (shuffle (range (count (:errors (first pop)))))]
    (if (or (empty? cases)
            (empty? (rest survivors)))
      (rand-nth survivors)
      (let
        [min-err-for-case
          (apply min
            (map #(nth % (first cases)) (map :errors survivors)))]
        (recur
          (filter
            #(= (nth (:errors %) (first cases)) min-err-for-case)
            survivors)
            (rest cases)
            )))))

(defn select-parent
  "Selects a parent from the population using the specified method."
  [pop argmap]
  (case (:parent-selection argmap)
    :tournament (tournament-selection pop argmap)
    :lexicase (lexicase-selection pop argmap)))
```

### Genetic operators

```klipse-cljs
(defn crossover
  "Crosses over two individuals using uniform crossover. Pads shorter one."
  [plushy-a plushy-b]
  (let [shorter (min-key count plushy-a plushy-b)
        longer (if (= shorter plushy-a)
                 plushy-b
                 plushy-a)
        length-diff (- (count longer) (count shorter))
        shorter-padded (concat shorter (repeat length-diff :crossover-padding))]
    (remove #(= % :crossover-padding)
            (map #(if (< (rand) 0.5) %1 %2)
                 shorter-padded
                 longer))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the plushy) with some probability."
  [plushy instructions]
  (let [rand-code (repeatedly (inc (count plushy))
                              (fn []
                                (if (< (rand) 0.05)
                                  (rand-nth instructions)
                                  :mutation-padding)))]
    (remove #(= % :mutation-padding)
            (interleave (conj plushy :mutation-padding)
                        rand-code))))

(defn uniform-deletion
  "Randomly deletes instructions from plushy at some rate."
  [plushy]
  (remove (fn [x] (< (rand) 0.05))
          plushy))
```

### Creating new (random) individuals

```klipse-cljs
(defn new-uuid
  "conditional UUID producer"
  []
  (str (random-uuid)))

(defn new-individual
  "Returns a new individual produced by selection and variation of
  individuals in the population."
  [pop argmap]
  {:id (new-uuid)
   :plushy
   (let [prob (rand)]
     (cond
       (< prob 0.5) (crossover (:plushy (select-parent pop argmap))
                               (:plushy (select-parent pop argmap)))
       (< prob 0.75) (uniform-addition (:plushy (select-parent pop argmap))
                                       (:instructions argmap))
       :else (uniform-deletion (:plushy (select-parent pop argmap)))))})

(defn make-random-plushy
  "Creates and returns a new plushy."
  [instructions max-initial-plushy-size]
  (repeatedly (rand-int max-initial-plushy-size)
              #(rand-nth instructions)))

(defn random-individual
  "Produce one random individual"
  [instructions max-size]
  (hash-map
    :id (new-uuid)
    :plushy
      (make-random-plushy instructions max-size)
      ))
```

### Create and manage populations

```klipse-cljs
(defn random-population
  "Produce a random population of the given size"
  [popsize instructions max-size]
  (repeatedly popsize #(random-individual instructions max-size)))

; TODO: Add *short* sleep here so that we can potentially pause, etc.,
; in the middle of evaluating a (possibly) large population. That probably
; requires going from the very nice `map` solution to a `loop-recur` approach,
; though, which is a bummer.

(defn score-sorted-population
  "Given a population and an error function, score the individuals
   (writing their scores and behaviors to them in the process) and
   sort by total-error"
  [population error-fxn args]
  (sort-by
    :total-error
    (map (partial error-fxn args) population)
    ))
```

### Persistence

These are "standard" Clojure atoms used to persist state across a run.

```klipse-cljs
(def population-atom (atom [])) ;; stores population between steps
(def current-total-error (atom nil)) ;; stores best total error from last gen
(def args-atom (atom {})) ;; stores arg hash
(def pause-atom (atom false)) ;; intended to be overridden externally
(def counter-atom (atom 0)) ;; to manage generation limits on runs

(def pushgp-results-atom (r/atom "<results will appear here>"))
(def pushgp-current-gen-atom (r/atom "<results will appear here>"))

; The overall state of the system
(def system-state (r/atom {}))

(defn initialize-state []
  (let [control-channel (async/chan)
        results-channel (async/chan)]
    (reset! system-state {
      :control-channel control-channel
      :results-channel results-channel
      ; Either :idle or :busy
      :computation-state :idle
      ; Either :stopped or :running or :shutdown
      :app-status :stopped})
    (reset! pushgp-results-atom "")
    (reset! pushgp-current-gen-atom "")
    (reset! population-atom [])
    (reset! current-total-error nil)
    (reset! counter-atom 0)))

@system-state
```

### Reporting

```klipse-cljs
(defn behavior-map
  [problem individual]
  (into (sorted-map) (zipmap (:args problem) (:behaviors individual))))

(defn report-generation
  "Generates a summary report for a given generation."
  [pop generation]
  (let [best (first pop)
        generation-report
          (str
            "\n-------------------------------------------------------\n"
            "               Report for Generation " generation "\n"
            "-------------------------------------------------------\n"
            "Best plushy: " (prn-str (:plushy best))
            "Best program: " (prn-str (push-from-plushy (:plushy best)))
            "Best total error: " (:total-error best) "\n"
            "Best errors: " (:errors best) "\n"
            "Behavior of best:\n"
            (behavior-map (:training-function @args-atom) best)
            "\n")]
      (swap! pushgp-results-atom str generation-report)
      (reset! pushgp-current-gen-atom generation-report)
      (reset! current-total-error (:total-error best))))

(defn report-starting-line
  [args]
  (swap! pushgp-results-atom str "Starting GP with args:\n" args "\n"))
```

### Evolutionary search

```klipse-cljs
(defn propel-setup!
  "Build an initial population using the specified arguments, placing it
   in the specified atom"
  [pop-atom popsize instructions max-size]
    (reset! pop-atom
      (random-population popsize instructions max-size)
      ))

(defn propel-population-step
  "Takes an existing population and a pile of arguments, and produces a
   next population of the same size, according to the specified parameters."
  [population argmap]
  (let
    [fxn (:fxn (:training-function argmap))
     errfxn (:error-function fxn)
     instructions (:instructions argmap)
     evaluated-pop (if (:errors (first population))
          population
          (score-sorted-population population errfxn argmap))]
    (repeatedly
      (:population-size argmap)
      #(new-individual evaluated-pop argmap)
      )))

(defn compute-errors
  [pop-atom arg-atom]
  ; In a more parallel universe this could be susceptible to a race condition
  ; because we check @pop-atom and then use it later, implicitly assuming it
  ; hasn't changed. In this setting it won't, but in a more parallel setting
  ; it could.
  (if (:errors (first @pop-atom))
     @pop-atom
     (score-sorted-population @pop-atom (:error-function @arg-atom) @arg-atom)))

(defn run-generation
  [pop-atom arg-atom counter-atom]
  (let [pop-size (:population-size @arg-atom)
        evaluated-pop (compute-errors pop-atom arg-atom)]
    (report-generation evaluated-pop @counter-atom)
    (swap! counter-atom inc)
    (reset! pop-atom
      (repeatedly pop-size
                  #(new-individual evaluated-pop @arg-atom)))))

(defn propel-gp!
  "Main GP loop, rewritten to use a population atom."
  [pop-atom arg-atom pause-atom counter-atom]
  (report-starting-line @arg-atom)
  (propel-setup! pop-atom
                 (:population-size @arg-atom)
                 (:instructions @arg-atom)
                 (:max-initial-plushy-size @arg-atom))
  (go-loop []
    (if-let [continue-token (async/<! (:control-channel @system-state))]
      (do
        (run-generation pop-atom arg-atom counter-atom)
        (async/>! (:results-channel @system-state) {:generation @counter-atom})
        (async/<! (async/timeout 10))
        (recur))
      ; If the result is nil then control channel has
      ; been shutdown, which means we should say the app has stopped
      ; and stop this loop.
      (swap! system-state assoc :app-status :stopped))))

(defn run-once
  "Run the program with the given initial state, until the step limit is reached."
  [program initial-state step-limit]
  (interpret-program program initial-state step-limit))
```

### Evaluating evolved programs

#### A few helper functions

These simplify construction of symbolic regression and classification
test problems.

```klipse-cljs
(defn program-behavior
  "Run the program with the specified input value (only one allowed), and
   return the item present at the top of the specified stack when done
   (or :no-stack-item if nothing is present there)."
  [program in-value behavior-stack step-limit]
  (peek-stack
    (run-once program
              (assoc empty-push-state :input {:in1 in-value})
              step-limit)
    behavior-stack))

(defn behavior-vector
  "Produces an ordered collection of behavior values taken from the
   specified stack top, using the specified inputs"
  [program fxn limit]
  (let [inputs (:args fxn)
        behavior-stack (:behavior fxn)]
    (map
      (fn [i] (program-behavior program i behavior-stack limit))
      inputs)))

(defn absolute-errors
  "Produces elementwise absolute difference between two numerical
   vectors (of the same length), or the penalty if no value is returned
   in the second."
  [expected observed penalty]
  (map
    (fn [v1 v2]
      (if (= :no-stack-item v2)
        penalty
        (abs (- v1 v2))
        ))
    expected
    observed))

(defn regression-error-function
  "Runs the individual's program over the input values specified by the
   problem, comparing the resulting behaviors (also specified in
   the :training-function) to the goals, and saving the behavior and
   error vectors, plus :total-error, in the individual. Returns an
   updated individual."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        fxn (:training-function argmap)
        limit (:step-limit argmap)
        penalty (:misbehavior-penalty argmap)
        outputs (behavior-vector program fxn limit)
        desired (map (:fxn fxn) (:args fxn))
        errors (absolute-errors desired outputs penalty)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply + errors))
           ))

(defn classification-errors
  "Produces 0 for each matching value, 1 for each mismatch, or the penalty
   if no value is returned at all. Basically Hamming Distance with a penalty
   for missing elements."
  [expected observed penalty]
  (map
    (fn [v1 v2]
      (if (= :no-stack-item v2)
        penalty
        (if (= v1 v2) 0 1)
        ))
    expected
    observed
    ))

(defn binary-classification-error-function
  "Runs the individual's program over the input values specified by the
   problem, comparing the resulting behaviors (also specified in
   the :training-function) to the goals, and saving the behavior and
   error vectors, plus :total-error, in the individual. Returns an
   updated individual."
  [argmap individual]
  (let [program (push-from-plushy (:plushy individual))
        fxn (:training-function argmap)
        limit (:step-limit argmap)
        penalty (:misbehavior-penalty argmap)
        outputs (behavior-vector program fxn limit)
        desired (map (:fxn fxn) (:args fxn))
        errors (classification-errors desired outputs penalty)]
    (assoc individual
           :behaviors outputs
           :errors errors
           :total-error (apply + errors))
           ))
```

#### Symbolic regression

```klipse-cljs
(def simple-quadratic-demo
  "Target function: f(x) = 7x^2 - 20x + 13, over the range [-10,11),
   with the result on :integer"
  {:fxn (fn [x] (+ (* 7 x x) (* -20 x) 13))
   :args (range -10 11)
   :behavior :integer
   :error-function regression-error-function})

(def simple-cubic-demo
  "Target function: f(x) = x^3 + x + 3, over the range [-10,11),
   with the result on :integer"
  {:fxn (fn [x] (+ (* x x x) x 3))
   :args (range -10 11)
   :behavior :integer
   :error-function regression-error-function})

(def birthday-args
  (take 10 (random-sample 0.01 (range))))

(def birthday-quadratic-demo
  "Target function: f(x) = 1964 - 11*x + 9x^2, over a random collection
   of integer arguments, with the result on :integer"
  {:fxn (fn [x] (+ (* 9 x x) (* -11 x) 1964))
   :args birthday-args
   :behavior :integer
   :error-function regression-error-function})

(def random-integer-pairs
  (let [xs (sort (take 4 (shuffle (range -10 10))))]
    (reduce #(assoc %1 %2 (rand-int 100)) {} xs)))

 (def random-integer-demo
   "Target function: random integers in [0,100) mapped to a random
    collection of 4 integer arguments, with the result on :integer"
   {:fxn #(get random-integer-pairs % 0)
    :args (keys random-integer-pairs)
    :behavior :integer
    :error-function regression-error-function})
```

#### String classification problems

```klipse-cljs
(def contains-T?-demo
  "Return true if the string contains at least one 'T' character, over
   the specified collection of inputs, with the result on :boolean"
  {:fxn (fn [s] (boolean (re-find #"T" s)))
   :args (sort ["GCG" "GACAG" "AGAAG" "CCCA" "GATTACA" "TAGG" "GACT"])
   :behavior :boolean
   :error-function binary-classification-error-function})

(defn random-string
  [letters max-size]
  (clojure.string/join
    (repeatedly
      (+ 2 (rand-int (- max-size 2)))
      #(rand-nth letters))))

(def string-args
  (sort (repeatedly 30 #(random-string ["A" "C" "G" "T"] 10))))

(def contains-TA-or-AT?-demo
  "Return true if the string contains substring 'TA' or 'AT', over a
   run-specific random sample of inputs, with the result on :boolean"
  {:fxn (fn [s] (or (boolean (re-find #"AT" s)) (boolean (re-find #"TA" s))))
   :args string-args
   :behavior :boolean
   :error-function binary-classification-error-function})

(def contains-CC-or-AT?-demo
  "Return true if the string contains substring 'CC' or 'AT', over a
   run-specific random sample of inputs, with the result on :boolean"
  {:fxn (fn [s] (or (boolean (re-find #"AT" s)) (boolean (re-find #"CC" s))))
   :args string-args
   :behavior :boolean
   :error-function binary-classification-error-function})
```

#### All the demo problems in one place

```klipse-cljs
(def demo-problems
  "Convenience to enable CLI specification of problem by keyword rather
   than symbol name"
  {:simple-cubic simple-cubic-demo
   :simple-quadratic simple-quadratic-demo
   :birthday-quadratic birthday-quadratic-demo
   :random-regression random-integer-demo
   :contains-T? contains-T?-demo
   :contains-TA-or-AT? contains-TA-or-AT?-demo
   :contains-CC-or-AT? contains-CC-or-AT?-demo
   })
```

### Argument management

This is a group of utility functions to help manage arguments to the run.

We almost certainly want to hide this.

```
;; CLI defaults:
;; - :instructions default-instructions
;;   - [specified collection of symbols]
;; - :target-problem :simple-cubic
;;   - :simple-quadratic
;;   - :birthday-quadratic
;;   - :random-integer-data
;;   - :contains-T?
;;   - :contains-TA-or-AT?
;; :population-size 200
;; :max-generations 100
;; :max-initial-plushy-size 50
;; :step-limit 100
;; :parent-selection :tournament
;;   - :lexicase
;; :misbehavior-penalty +1e12
;; :tournament-size 5
```

```klipse-cljs
(def default-args
  "Default argument hash, which is modified by a hash and CLI args."
   {:instructions default-instructions
    :target-problem :simple-cubic
    :population-size 200
    :max-initial-plushy-size 50
    :step-limit 100
    :parent-selection :tournament
    :misbehavior-penalty +1e12
    :tournament-size 5
    :max-generations 100
    })

(defn cljc-read-string
  [string]
  (cljs.reader/read-string string))

(defn parse-cli-args
  [arg-strings]
  (->> arg-strings
      (map cljc-read-string ,)
      (apply hash-map ,)
      ))

(defn update-derived-args
  [arg-hash]
  (let [demo (get demo-problems (:target-problem arg-hash) :UNKNOWN-PROBLEM)]
    (-> arg-hash
      (assoc , :training-function demo)
      (assoc , :error-function (:error-function demo))
      )))

(defn collect-the-args!
  "Omnibus function to merge all the arguments in play, and store them
   in the atom specified.

  If no optional arguments are passed in, the default-args result.
  The other named arguments are :cli-hash (for parsed arguments from a
  command line) and :override-hash (for a hash of arguments passed in
  programmatically). These are merged into default-args in that order.
  Finally, some necessary derived arguments are inserted."
  [arg-atom & {:keys [cli-hash override-hash] :or {cli-hash {} override-hash {}}}]
  (let [merged (merge default-args cli-hash override-hash)]
    (reset! arg-atom (update-derived-args merged))))
```

### Set-up and run Propel

```klipse-cljs
(defn setup-and-run-propel!
  [& args]
  (initialize-state)
  (collect-the-args! args-atom
    :cli-hash (parse-cli-args args))
  (println @args-atom)
  (propel-gp! population-atom
              args-atom
              pause-atom
              counter-atom))

(setup-and-run-propel! ":population-size" "100" ":max-generations" "20")
```

### Start the control loop

```klipse-cljs
(defn process-result [generation]
  (if (= :running (:app-status @system-state))
    (go
      ; We need this timeout so the computational side of the
      ; system will sleep for a little, giving the UI side a
      ; little access to the CPU to process user input.
      (async/<! (async/timeout 10))
      (async/>! (:control-channel @system-state) :run-next))
    (swap! system-state assoc :computation-state :idle)))

(defn control-loop
  []
  (go-loop []
    (when-let [{generation :generation} (async/<! (:results-channel @system-state))]
      (if (zero? @current-total-error)
        (do
          (swap! system-state assoc :app-status :shutdown))
        (do
          (process-result generation)
          (recur))))))

(control-loop)
```

### Build the UI

```klipse-reagent
(defn stopped->running []
  (swap! system-state assoc :app-status :running)
  (when (= :idle (:computation-state @system-state))
    (swap! system-state assoc :computation-state :busy)
    (go
      (async/>! (:control-channel @system-state) :run-next))))

(defn running->stopped []
  (swap! system-state assoc :app-status :stopped))

(defn handle-play-pause [event]
  (case (:app-status @system-state)
    :stopped (stopped->running)
    :running (running->stopped)))

(defn reset-system [event]
  (async/close! (:control-channel @system-state))
  (async/close! (:results-channel @system-state))
  (initialize-state)
  (setup-and-run-propel! ":population-size" "100" ":max-generations" "20")
  (control-loop))

(defn pushgp-current-gen-component []
  [:div {:id "pushgp-current-gen"}
    [:hr]
    [:h2 "Current generation"]
    [:p @pushgp-current-gen-atom]])

(defn pushgp-results-component []
  [:div {:style {:height "400px" :max-height "400px" :overflow "auto"}
         :id "pushgp-results"}
    [:hr]
    [:h2 "All generations"]
    [:p @pushgp-results-atom]])

(defn play-pause-button [app-status]
  [:button {:on-click handle-play-pause}
    (if (= app-status :running)
      [:i {:class "fa fa-pause-circle" :style {:color "red" :font-size "200%"}}]
      [:i {:class "fa fa-play-circle" :style {:color "green" :font-size "200%"}}])])

(defn pushgp-output-component []
  [:div
    ; It's useful to put the button _above_ the output, otherwise it keeps
    ; getting pushed down the page and you end up having to chase it it you
    ; want to stop the thing.
    (when-not (= :shutdown (:app-status @system-state))
      [play-pause-button (:app-status @system-state)])
    (when (not-empty @system-state)
      [:button
        {:on-click reset-system :style {:color "red" :font-size "150%"}}
        "Reset"])
    [pushgp-current-gen-component]
    [pushgp-results-component]])

[pushgp-output-component]
```
