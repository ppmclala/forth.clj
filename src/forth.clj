(ns forth
  (:import [java.util Scanner]))

(def ^:dynamic *debug* false)
(defn debug [{:keys [stack dict mode compile-target custom-dict] :as machine}]
  (when *debug*
    (println "Current machine: ")
    (println "\tstack:      top->" @stack)
    (println "\tmode:            " @mode)
    (when (= :compile @mode)
      (let [instrs (get @custom-dict @compile-target)]
        (println "\tcompile-target:  " @compile-target)
        (println "\tinstructions:    " (when instrs @instrs))))))

(defn print-dict [{:keys [dict]}]
  (println "Dictionary:\n\tdict:       " (keys @dict)))

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
    ":" :DOCOL
    ";" :EXIT
    t))

(defn- begin-comment [m] (reset! m :comment))
(defn- end-comment [m] (reset! m :interpret))
(defn- start-compiling [m] (reset! m :compile))
(defn- stop-compiling [{:keys [mode compile-target]}]
  (reset! mode :interpret)
  (reset! compile-target nil))

;; intrinsics
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
   :END_COMMENT (fn [_ _ m] (end-comment m))
   :DOCOL (fn [_ _ m] (start-compiling m))
   :EXIT (fn [_ _ m] (stop-compiling m))})

(defn initialize []
  {:stream (Scanner. System/in)
   :stack (atom (list))
   :mode (atom :interpret) ; maybe :eval?
   :compile-target (atom nil)
   :custom-dict (atom {})
   :dict (atom native-words)})

(defn- unhandled-word [m w]
  (println "Don't know how to handle word " w)
  (print-dict m))

(defn- set-compile-target [compile-target custom-dict word]
  (reset! compile-target word)
  (swap! custom-dict assoc word (atom [])))

(defn- compile-word [instructions w]
  (conj instructions w))

(defn- compile-next [{:keys [compile-target custom-dict]} word]
  (if-not @compile-target
    (set-compile-target compile-target custom-dict word)
    (swap! (get @custom-dict @compile-target) compile-word word)))

(defn- custom-instrs [{:keys [custom-dict]} word]
  (get @custom-dict word))

(defn- exec-instrs* [{:keys [dict stack mode] :as m} instrs]
  (let [next-word (first instrs)
        custom-instrs (custom-instrs m next-word)
        rest-instrs (rest instrs)]
    (if custom-instrs
      (do
        (exec-instrs* m @custom-instrs)   ;; execute custom word
        (exec-instrs* m rest-instrs))   ;; continue with next instructions
      (let [native-fn (get @dict next-word)]
        (if native-fn
          (do
            (native-fn stack dict mode)
            (exec-instrs* m rest-instrs))
          (when next-word 
            (unhandled-word m next-word)))))))

(defn- next-word [{:keys [stack dict mode] :as m} w]
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

(defn forth-eval [{:keys [stack] :as machine} t]
  (if (instance? Long t)
    (!push stack t)
    (next-word machine (token->word t)))
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
  ;; - [ ] implment DOCOL (implies compiling mode)

  ;;
  )
