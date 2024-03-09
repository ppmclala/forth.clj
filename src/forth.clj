(ns forth
  (:import [java.util Scanner]))

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

(defn- !peek [ds] (first @ds))

(def ^:private native-words
  {:+ (fn [s _] (!push s (+ (!pop s) (!pop s))))
   :. (fn [s _] (println (!pop s)))})

(defn initialize []
  (let [s (Scanner. System/in)
        ds (atom (list))]
    {:stream s
     :stack ds
     :dict native-words}))

(defn- unhandled-word [m w]
  (println "Don't know how to handle word " w)
  (println "Current machine: " (dissoc m :stream)))

(defn- evaluate-word [{:keys [stack dict] :as m} w]
  (let [f (get dict w)]
    (if f (f stack dict) (unhandled-word m w))))

(defn forth-eval [{:keys [stack] :as machine} t]
  (if (instance? Long t)
    (!push stack t)
    (evaluate-word machine t))
  machine)

(defn eval-line [machine line]
  (let [scanner (Scanner. line)]
    (loop [m machine
           t (next-token scanner)]
      (if t (recur (forth-eval m t) (next-token scanner)) m))))

(defn- next-line [{:keys [stream]}] (. stream nextLine))

(defn repl [machine]
  (print "forth.clj> ")
  (flush)
  (let [l (next-line machine)]
    (repl (eval-line machine l))))

(defn -main []
  (repl (initialize)))

(comment

  (repl (initialize))

  "1 1 +"

  (first (list 1 2 3))
  (conj (list 1 2 3) 4)

  (defn foo [x y] (println x y))
  (apply foo {:a 1 :b 2})

  (def stack* (atom (list)))

  (!push stack* 1)
  @stack*
  (!push stack* 2)
  (!pop stack*)

  ;;
  )
