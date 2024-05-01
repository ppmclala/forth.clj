(ns forth
  (:import [java.util Scanner]))

(defn- !push [ds & tokens]
  (swap! ds (fn [s t] (apply conj s t)) tokens))

(defn- !peek [ds] (first @ds))

(defn- !pop [ds]
  (let [v (!peek ds)] (swap! ds (fn [s] (rest s))) v))

(defn- begin-comment [m] (reset! m :comment))
(defn- end-comment [m] (reset! m :interpret))
(defn- start-compiling [m] (reset! m :compile))
(defn- stop-compiling [{:keys [mode compile-target]}]
  (reset! mode :interpret)
  (reset! compile-target nil))

(defn binop [s op]
  (let [*1 (!pop s)
        *2 (!pop s)]
    (!push s (op *2 *1))))

(defn compop [s c]
  (let [*1 (!pop s)
        *2 (!pop s)]
    (!push s (if (c *2 *1) -1 0))))

;; intrinsics
(def dictionary
  {:.
   (fn [s _ _ _] (println (!pop s)))

   :+
   (fn [s _ _ _] (binop s +))

   :-
   (fn [s _ _ _] (binop s -))

   :*
   (fn [s _ _ _] (binop s *))

   :MOD
   (fn [s _ _ _]
     (let [*1 (!pop s)
           *2 (!pop s)]
       (!push s (mod *2 *1)) ; remainder
       (!push s (quot *2 *1)))) ; quotient

   :DUP
   (fn [s _ _ _] (!push s (!peek s)))

   :SWAP
   (fn [s _ _ _]
     (let [*1 (!pop s)
           *2 (!pop s)]
       (!push s *1 *2)))

   :ROT
   (fn [s _ _ _]
     (let [*1 (!pop s)
           *2 (!pop s)
           *3 (!pop s)]
       (!push s *3 *1 *2)))
   :DROP
   (fn [s _ _ _] (!pop s))

   :BEGIN_COMMENT
   (fn [_ _ m _] (begin-comment m))

   :END_COMMENT
   (fn [_ _ m _] (end-comment m))

   :DOCOL
   (fn [_ _ m _] (start-compiling m))

   :EXIT
   (fn [_ _ m _] (stop-compiling m))
   
   :!
   (fn [s _ _ mem] 
     (let [addr (!pop s)
           val (!pop s)]
       (swap! mem assoc addr val)))
   
   :AT
   (fn [s _ _ mem] 
     (->> (!pop s) (get @mem) (!push s)))
   
   :+!
   (fn [s _ _ mem]
     (let [addr (!pop s)
           val (!pop s)]
       (swap! mem update addr + val)))
   
   :<
   (fn [s _ _ _] (compop s <))
     
   :>
   (fn [s _ _ _] (compop s >))

   :=
   (fn [s _ _ _] (compop s -))

   :>=
   (fn [s _ _ _] (compop s >=))

   :<=
   (fn [s _ _ _] (compop s <=))})

(defn inspect [{:keys [stack mode dict memory compile-target]}]
  (println "Current machine: ")
  (println "\tstack:      top->" @stack)
  (println "\tmode:            " @mode)
  (println "\tmemory:          " @memory)
  (when (= :compile @mode)
    (let [instrs (get @dict @compile-target)]
      (println "\tcompile-target:  " @compile-target)
      (println "\tinstructions:    " (when instrs @instrs)))))

(defn print-dict [{:keys [dict]}]
  (println "Dictionary:\n\tdict:       " (keys @dict)))

(defn next-token [stream]
  (cond
    (. stream hasNextBigInteger) (. stream nextBigInteger)
    (. stream hasNext) (keyword (. stream next))))

(defn- match-kw [kw t] (= t (keyword kw)))

(defn- token->word [t]
  (condp match-kw t
    "(" :BEGIN_COMMENT
    ")" :END_COMMENT
    ":" :DOCOL
    ";" :EXIT
    "@" :AT
    t))

(defn- unhandled-word [m w]
  (println "Don't know how to handle word " w)
  (print-dict m))

(defn- set-compile-target [compile-target dict word]
  (reset! compile-target word)
  (swap! dict assoc word (atom [])))

(defn- compile-next [{:keys [compile-target dict]} word]
  (if-not @compile-target
    (set-compile-target compile-target dict word)
    (swap! (get @dict @compile-target) conj word)))

(defn- compiled? [h] (= (type h) clojure.lang.Atom))
(defn- native? [h] (fn? h))

(defn- exec-instrs* [{:keys [dict stack mode memory] :as m} instrs]
  (let [next-word (first instrs)
        code-word (get @dict next-word)]

    (cond
      (compiled? code-word)
      ;; code-word is a vector of instructions (wrapped in an Atom)
      (do
        (exec-instrs* m @code-word)
        (exec-instrs* m (rest instrs)))

      (native? code-word)
      ;; code-word is a fn
      (do
        (code-word stack dict mode memory)
        (exec-instrs* m (rest instrs)))

      (some? next-word)
      (unhandled-word m next-word))))

(defn- next-word [{:keys [mode] :as m} w]
  (case @mode
    :comment
    (when (= w :END_COMMENT)
      (end-comment mode))

    :interpret
    (exec-instrs* m [w])

    :compile
    (if (= w :EXIT)
      (stop-compiling m)
      (compile-next m w))))

(defn- forth-eval [{:keys [stack] :as machine} t]
  (if (instance? BigInteger t)
    (!push stack t)
    (next-word machine (token->word t)))
  machine)

(defn- eval-line [machine line]
  (let [scanner (Scanner. line)]
    (loop [m machine
           t (next-token scanner)]
      (if t (recur (forth-eval m t) (next-token scanner)) m))))

(defn initialize []
  {:stream (Scanner. System/in)
   :stack (atom (list))
   :rstack (atom (list))
   :mode (atom :interpret) ; maybe :eval?
   :compile-target (atom nil)
   :dict (atom dictionary)
   :memory (atom {})})

(defn repl [{:keys [stream] :as machine}]
  (inspect machine)
  (print "forth.clj> ")
  (flush)
  (->>
   (. stream nextLine)
   (eval-line machine)
   repl))

(defn -main []
  (repl (initialize)))

(comment

  (repl (initialize))

  ;; 4 3 > if 9 else 0 end

  ;;
  )
