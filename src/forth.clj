(ns forth
  (:require
   [clojure.repl :refer [demunge source-fn]])
  (:import
   [java.util Scanner]))

(def ^:dynamic *debug* false)
(defn debug [{:keys [stack dict]}]
  (when *debug*
    (println "Current machine: ")
    (println "\tstack: " @stack)
    (println "\tdict: " (keys dict))))

(defn next-token [stream]
  (when (. stream hasNext)
    (let [raw-token (. stream next)]
      (try
        (Long/parseLong raw-token)
        (catch Exception _
          (keyword raw-token))))))

(defn !push [ds token]
  (swap! ds (fn [s t] (conj s t)) token))

(defn !pop [ds]
  (let [v (first @ds)]
    (swap! ds (fn [s] (rest s)))
    v))

(defn !peek [ds] (first @ds))

;; the clojure LSP hates this one simple trick! (crashes on :/ keyword)
(defn translate-token [t] (if (= t (keyword "/")) :DIV t))

(def ^:private native-words
  {:. (fn [s _] (println (!pop s)))
   :+ (fn [s _] (!push s (+ (!pop s) (!pop s))))
   :- (fn [s _]
        (let [*1 (!pop s)
              *2 (!pop s)]
          (!push s (- *2 *1))))
   :* (fn [s _] (!push s (* (!pop s) (!pop s))))
   :DIV (fn [s _]
        (let [*1 (!pop s)
              *2 (!pop s)]
          (!push s (/ *2 *1))))

   :DUP (fn [s _] (!push s (!peek s)))
   :SWAP (fn [s _] 
           (let [*1 (!pop s) 
                 *2 (!pop s)] 
             (!push s *1) 
             (!push s *2)))})

(defn initialize []
  (let [s (Scanner. System/in)
        ds (atom (list))]
    {:stream s
     :stack ds
     :dict native-words}))

(defn- unhandled-word [m w]
  (println "Don't know how to handle word " w)
  (debug m))

(defn- evaluate-word [{:keys [stack dict] :as m} w]
  (let [f (get dict w)]
    (if f (f stack dict) (unhandled-word m w))))

(defn forth-eval [{:keys [stack] :as machine} t]
  (if (instance? Long t)
    (!push stack t)
    (evaluate-word machine (translate-token t)))
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

  (-> (:+ native-words) .getClass .getName demunge symbol ;;source-fn
      )

  ;;
  )
