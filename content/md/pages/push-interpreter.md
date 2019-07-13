{:title "Simple Push interpreter"
 :layout :page
 :page-index 1
 :home? true
 :navbar? false
 :klipse true}

Here we'll include the code for a simple Push interpreter (written in Clojure)
that's completely live. You can edit the examples get instant feedback, and
actually edit the interpreter if you're so inclined.

## Setting things up

This sets up the Clojure(Script) namespace and dependencies. It's all
entirely Clojure specific, so feel free to ignore it.

```klipse-cljs
(ns propel.core
  (:require
    [clojure.string :as string]
    [reagent.core :as r]))
```

The state of this simple Push interpreter will just be a map containing
the contents of the various stacks we're using (e.g., `:exec` and `:integer`),
along with `:input`, which is a map of input parameters to their values. All
of these are empty by default.

```klipse-cljs
(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :input {}})
```

## Stack manipulation

Here we define the basic stack manipulation functions. The implementation
details are a bit Clojure specific in places, but they all do the "obvious"
thing that you'd want for a stack datatype.

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
```

This function is a bit more complex. It takes a Push state and a list of stacks
that indicate where we're taking arguments from. It checks to see if the
stacks contain values of the desired types. If it doesn't we return
`:not-enough-args`. If all the desired arguments do exist, then it returns
the list of those argument values, along with the new Push state with those
arguments popped off the appropriate stacks.

```klipse-cljs
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
```

Try this out below. We have a Push state with some values on the stacks,
and a call to `get-args-from-stacks` with a few inputs. Feel free to change
the contents of the stack and/or the inputs and see how that changes the result.

```klipse-cljs
(def sample-push-state
  {:exec '()
   :integer '(5 8 9)
   :string '("GECCO" "PushGP")
   :boolean '(false true)
   :input {}})

(get-args-from-stacks
  sample-push-state
  [:integer :boolean :string :integer :integer :string])
```

This one is a helper function that simplifies the construction of new
instructions by managing all the stack manipulation. It is used heavily
in the construction of instructions below.

```klipse-cljs
(defn build-push-instruction
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

## Instructions

In this section we'll walk through constructing Push instructions that can
be "understood" by the Push interpreter. To keep things simple we'll
illustrate instructions that act on two different types, `integer` and
`boolean`, alone with a small number of `exec` instructions. There are
hundreds of other instructions on these and other types, but we don't to
completely overwhelm you here.

### "Reading" input parameters

This instruction reads the value of the (single, for now) input argument
(e.g., the input _x_ for a symbolic regression problem) and pushes it onto
the `:exec` stack. It will then get transferred to the stack of the appropriate
type when "executed" as an "instruction".

```klipse-cljs
(defn in1
  "Pushes the input labeled :in1 from the inputs map onto the :exec stack."
  [state]
  (push-to-stack state :exec (:in1 (:input state))))
```

Here's an example of this instruction in action. Feel free to edit the state
and the input value to see how they change the outcome.

```klipse-cljs
(def sample-input-push-state
  {:exec '()
   :integer '(5 8 9)
   :string '("GECCO" "PushGP")
   :boolean '(false true)
   :input {:in1 "I am an input!"}})

(in1 sample-input-push-state)
```

### Integer instructions

A bunch of integer instructions that support:
   * Incrementing and decrementing
   * Addition, subtraction, multiplication, and protected division
      * We define protected division to return 1 if the denominator is 0.
   * Equality, which takes two integers and pushes a _boolean_ result.
   * `Dup` and `Swap`, which manipulate the contents of the `:integer` stack.
      * `integer_dup` creates a second copy of whatever is on the top of the
        `:integer` stack.
      * `integer_swap` swaps the order of the top two elements of the
        `:integer` stack.
      * These kind of stack manipulation instructions are repeated for the
        other types (e.g., `:boolean` and `:string`) as well.

Each of these calls `build-push-instruction` from above, providing the Push
state being manipulated the Clojure function that will actually be called
to evaluate this Push instruction, a vector of stacks that the arguments come
from, and the stack the result is pushed onto. Many of these vary only in the
Clojure function that should be called when this instruction is executed.

```klipse-cljs
(defn integer_inc
  [state]
  (build-push-instruction
    state inc [:integer] :integer))

(defn integer_dec
  [state]
  (build-push-instruction
    state dec [:integer] :integer))

(defn integer_+
  [state]
  (build-push-instruction
    state + [:integer :integer] :integer))

(defn integer_-
  [state]
  (build-push-instruction
    state - [:integer :integer] :integer))

(defn integer_*
  [state]
  (build-push-instruction
    state * [:integer :integer] :integer))

(defn integer_%
  [state]
  (build-push-instruction state
                         (fn [int1 int2]
                           (if (zero? int2)
                             1
                             (quot int1 int2)))
                         [:integer :integer]
                         :integer))

(defn integer_=
  [state]
  (build-push-instruction state = [:integer :integer] :boolean))

(defn integer_dup
  [state]
  (build-push-instruction
    state #(list % %) [:integer] :integer))

(defn integer_swap
  [state]
  (build-push-instruction
    state #(list %1 %2) [:integer :integer] :integer))
```

To try this out, let's define a Push state and see what happens when we
execute an instruction on that state. Feel free to edit the state and the
instruction to see what happens.

```klipse-cljs
(def sample-integer-push-state
  {:exec '()
   :integer '(5 8 9)
   :string '("GECCO" "PushGP")
   :boolean '(false true)
   :input {}})

(integer_- sample-integer-push-state)
```

### Exec stack instructions

The first two `:exec` stack instructions are `Dup` and `Swap` just like we
have for integers above.

`exec_if` is a little more complicated. It looks at the top value of the
`:boolean` stack and, depending on that value removes either the top or the
second-from-top value on the `:exec` stack. If you think of those two values
as being the `then` and `else` blocks of a traditional `if` statement, we're
just removing the one that we _don't_ want to execute, leaving the other on
the `:exec` stack to be executed in the next step.

```klipse-cljs
(defn exec_dup
  [state]
  (build-push-instruction
    state #(list % %) [:exec] :exec))

(defn exec_swap
  [state]
  (build-push-instruction
    state #(list %1 %2) [:exec :exec] :exec))

(defn exec_if
  [state]
  (build-push-instruction state
                         #(if %1 %3 %2)
                         [:boolean :exec :exec]
                         :exec))
```

To see `exec_if` in action, play with the following snippet which uses the
top of the boolean stack to decide whether to increment or decrement an
integer.

```klipse-cljs
(def sample-exec-push-state
  {:exec '(integer_inc integer_dec integer_+)
   :integer '(5 8 9)
   :string '("GECCO" "PushGP")
   :boolean '(false true)
   :input {}})

(exec_if sample-exec-push-state)
```

### Boolean Instructions

```klipse-cljs
(defn boolean_and
  [state]
  (build-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  [state]
  (build-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  [state]
  (build-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  [state]
  (build-push-instruction state = [:boolean :boolean] :boolean))

(defn boolean_dup
  [state]
  (build-push-instruction
    state #(list % %) [:boolean] :boolean))

(defn boolean_swap
  [state]
  (build-push-instruction
    state #(list %1 %2) [:boolean :boolean] :boolean))
```

### All together, now…

This brings all the instructions together in one map, and also adds in
a set of constants of different types.

```klipse-cljs
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
     ]))
```

## The actual Push interpreter

First, a few utility functions…

```klipse-cljs
(defn push-instruction?
  [item]
  (contains? push-instruction-registry item))

(defn cljc-throw
  [item]
  (throw
    (js/Error.
      (str
        "Unrecognized Push instruction in program: "
        item))))
```

### Interpret one instruction

Here we "execute" whatever is on top of the `:exec` stack. If it's an
instruction then we execute it by popping arguments from the input stacks, etc.
If it's a constant then we just move it to the stack for that type. If it's
a sequence then it's a block of instructions (or blocks) and we pop each of
those items onto the `:exec` stack individually.

```klipse-cljs
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
```

Here we illustrate the use of `interpret-one-step`; feel free to play with
the initial state (including the program on the `:exec` stack) to see how
the interpreter works.

```klipse-cljs
(def sample-one-step-push-state
  {:exec '(integer_dup integer_= exec_if integer_inc integer_dec integer_*)
   :integer '(5 8 9)
   :string '("GECCO" "PushGP")
   :boolean '()
   :input {}})

(interpret-one-step sample-one-step-push-state)
```

### Run a program!

`interpret-program` runs the given program in the given initial state for
at most `step-limit` steps, i.e., it won't call `interpret-one-step` more than
`step-limit` times.

```klipse-cljs
(defn interpret-program
  "Runs the given problem starting with the stacks in start-state."
  [program start-state step-limit]
  (loop [state (assoc start-state :exec program :step 1)]
    (if (or (empty? (:exec state))
            (> (:step state) step-limit))
      state
      (recur (update (interpret-one-step state) :step inc)))))
```

Let's play with the interpreter. Here we have an input state with some
integers on the `:integer` stack, along with a simple program,
and `interpret-program` returns the resulting state. Feel free to modify
both the start state and the program to see how the interpreter behaves.

```klipse-cljs
(def sample-interpreter-push-state
  {:exec '()
   :integer '(5 8 9)
   :string '("GECCO" "PushGP")
   :boolean '()
   :input {}})

(interpret-program
  '(integer_dup integer_= exec_if integer_inc integer_dec integer_*)
  sample-interpreter-push-state
  100)
```
