(ns forth
  (:import [java.util Scanner]))

(def ^:dynamic *debug* false)
(defn debug [{:keys [stack dict mode]}]
  (when *debug*
    (println "Current machine: ")
    (println "\tstack: top->" @stack)
    (println "\tdict:       " (keys @dict))
    (println "\tmode:       " @mode)))

(defn next-token [stream]
  (when (. stream hasNext)
    (let [raw-token (. stream next)]
      (try
        (Long/parseLong raw-token)
        (catch Exception _
          (keyword raw-token))))))

(defn !push [ds & tokens]
  (swap! ds (fn [s t] (apply conj s t)) tokens))

(defn !pop [ds]
  (let [v (first @ds)]
    (swap! ds (fn [s] (rest s)))
    v))

(defn !peek [ds] (first @ds))

(defn match-kw [kw t] (= t (keyword kw)))

(defn token->word [t]
  (condp match-kw t
    "/" :DIV
    "(" :BEGIN_COMMENT
    ")" :END_COMMENT
    t))

(defn- begin-comment [m] (reset! m :comment))
(defn- end-comment [m] (reset! m :interpret))

(def native-words
  {:. (fn [s _ _] (println (!pop s)))
   :+ (fn [s _ _] (!push s (+ (!pop s) (!pop s))))
   :- (fn [s _ _]
        (let [*1 (!pop s)
              *2 (!pop s)]
          (!push s (- *2 *1))))
   :* (fn [s _ _] (!push s (* (!pop s) (!pop s))))
   :DIV (fn [s _ _]
          (let [*1 (!pop s)
                *2 (!pop s)]
            (!push s (/ *2 *1))))

   :DUP (fn [s _ _] (!push s (!peek s)))
   :SWAP (fn [s _ _]
           (let [*1 (!pop s)
                 *2 (!pop s)]
             (!push s *1 *2)))
   :ROT (fn [s _ _]
          (let [*1 (!pop s)
                *2 (!pop s)
                *3 (!pop s)]
            (!push s *3 *1 *2)))
   :DROP (fn [s _ _] (!pop s))
   :BEGIN_COMMENT (fn [_ _ m] (begin-comment m))
   :END_COMMENT (fn [_ _ m] (end-comment m))})

(defn initialize []
  {:stream (Scanner. System/in)
   :stack (atom (list))
   :mode (atom :interpret) ; maybe :eval?
   :dict (atom native-words)})

(defn- unhandled-word [m w]
  (println "Don't know how to handle word " w)
  (debug m))

(defn- evaluate-word [{:keys [stack dict mode] :as m} w]
  (case @mode
    :comment
    (when (= w :END_COMMENT)
      (end-comment mode))

    :interpret
    (let [f (get @dict w)]
      (if f
        (f stack dict mode)
        (unhandled-word m w)))))

(defn forth-eval [{:keys [stack] :as machine} t]
  (if (instance? Long t)
    (!push stack t)
    (evaluate-word machine (token->word t)))
  machine)

(defn eval-line [machine line]
  (let [scanner (Scanner. line)]
    (loop [m machine
           t (next-token scanner)]
      (if t (recur (forth-eval m t) (next-token scanner)) m))))

(defn- next-line [{:keys [stream]}] (. stream nextLine))

(defn repl [machine]
  (with-bindings {#'*debug* true}
    (debug machine)
    (print "forth.clj> ")
    (flush)
    (let [l (next-line machine)]
      (repl (eval-line machine l)))))

(defn -main []
  (repl (initialize)))

(comment

  (repl (initialize))

  ;; test case:
  ;; : SQ ( n -- s ) DUP * ;
  ;; 2 SQ ;-> 4

  ;; TODO:
  ;; - [x] implement comments (implies modes during tokenization)
  ;; - [ ] implment DOCOL (implies compiling mode)

  ;;
  )
