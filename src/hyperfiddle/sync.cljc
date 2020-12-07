(ns hyperfiddle.sync
  (:require [hyperfiddle.fabric :as f :refer [Just Nothing]]
            [minitest :refer [tests]]))

(defn- deliver-value!
  "Manually deliver a `value` to a (potentially pending) push `node`."
  [node value]
  (let [Flow #?(:clj  (hyperfiddle.Origin/get)
                :cljs (.get f/Origin))]
    (.resume Flow node (f/Val (f/unwrap value)))))

(defn- deliver-to-queue!
  "Given a queue atom and a trace element, deliver the value to the potentially
  corresponding node in the queue. Noop if the corresponding node is not
  queued."
  [aqueue [node-id value]]
  (swap! aqueue (fn [queue]
                  (if-let [node (get queue node-id)]
                    (do (deliver-value! node value)
                        (dissoc queue node-id))
                    queue))))

(defn- deliver-trace-element!
  "Saves the given trace element in `atrace` atom and tries to deliver the value
  to a potentially queued node."
  [aqueue atrace trace-element]
  (deliver-to-queue! aqueue trace-element)
  (swap! atrace assoc (first trace-element) (second trace-element)))

(defn- enqueue! [aqueue id node]
  (swap! aqueue assoc id node)
  nil)

(defn- pop-trace! [atrace id]
  (when-let [v (get @atrace id)]
    ;; (swap! atrace dissoc id)
    v))

(defn remote-executor [aqueue atrace putf]
  (let [next-id (f/counter)]
    (fn [& [this & args]]
      (let [id (or (.-id this) (next-id))]
        (set! (.-id this) id)
        (let [trace-value (pop-trace! atrace id)]
          (case (f/node-type this)
            "From"   (let [value (first args)];; (put >a value)
                       (or trace-value
                           (do (putf value)
                               (enqueue! aqueue id this)
                               (Nothing))))
            "ApplyN" (or trace-value
                         (do (enqueue! aqueue id this)
                             (Nothing)))))))))

(def ^:private noop (constantly nil))


(tests
 ;; Client side node is queued BEFORE the server sends a value
 !! (def aqueue (atom {}))
 !! (def atrace (atom {}))
 !! (def >n0 (f/input))
 !! (def >n1 (f/fmap inc >n0))
 !! (def >n2 (f/fmap inc >n1))
 !! (def >out (f/cap >n2))
 !! (f/with-executor (remote-executor aqueue atrace noop)
      (f/put >n0 0)
      (deliver-trace-element! aqueue atrace [0 (Just 0)]) ;; [id value]
      (deliver-trace-element! aqueue atrace [1 (Just 1)])
      (deliver-trace-element! aqueue atrace [2 (Just 2)]))
 @>out => 2
 @atrace => {0 (Just 0)
             1 (Just 1)
             2 (Just 2)})

(tests
 ;; Client side node is queued AFTER the server sends a value
 !! (def aqueue (atom {}))
 !! (def atrace (atom {}))
 !! (def >n0 (f/input))
 !! (def >n1 (f/fmap inc >n0))
 !! (def >n2 (f/fmap inc >n1))
 !! (def >out (f/cap >n2))
 !! (f/with-executor (remote-executor aqueue atrace noop)
      (deliver-trace-element! aqueue atrace [2 (Just 2)])
      (deliver-trace-element! aqueue atrace [1 (Just 1)])
      (deliver-trace-element! aqueue atrace [0 (Just 0)])
      (f/put >n0 0))
 @>out => 2
 @atrace => {0 (Just 0)
             1 (Just 1)
             2 (Just 2)})


(tests
 ;; Value round trips between client and server
 !! (def aqueue (atom {}))
 !! (def atrace (atom {}))
 !! (def >n0 (f/input))
 !! (def >n1 (f/fmap inc >n0))
 !! (def >out (f/cap >n1))
 !! (deliver-trace-element! aqueue atrace [1 1]) ;; [id value]
 ;; ;; Client side
 !! (f/with-executor (remote-executor aqueue atrace
                                      (fn putf [value]
                                        ;; Server side
                                        (Thread/sleep 100) ;; simulate server/network latency
                                        (f/with-executor (f/tracing-executor #(deliver-trace-element! aqueue atrace %))
                                          (f/put >n0 value)
                                          (assert (= 1 @>out)))))
      (f/put >n0 0))
 @>out => 1
 @atrace => {0 (Just 0),
             1 (Just 1)})

(defn remote-executor [aqueue atrace putf]
  (let [next-id (f/counter)]
    (fn [& [this & args]]
      (if-not (.-shared this)
        (apply f/compute-executor this args)
        (let [id (or (.-id this) (next-id))]
          (set! (.-id this) id)
          (let [trace-value (get @atrace id)]
            (case (f/node-type this)
              "From"   (let [value (first args)];; (put >a value)
                                  (or trace-value
                                      (do (putf value)
                                          (enqueue! aqueue id this)
                                          (Nothing))))
              "ApplyN" (or trace-value
                           (do (enqueue! aqueue id this)
                               (Nothing))))))))))


(tests
 ;; Some nodes are shared, some are not.
 ;; Local (non-shared) nodes:
 ;;  - should compute on their own (apply)
 ;;  - should’n appear in the trace.

 ;; >n0
 ;; |- >n1
 ;; |  |- >n2
 ;; |     |- >out
 ;; |
 ;; |- >n-local
 ;; |- >out-local

 !! (def aqueue (atom {}))
 !! (def atrace (atom {}))
 !! (def >n0      (f/shared (f/input)))
 !! (def >n1      (f/shared (f/fmap inc >n0)))
 !! (def >n-local (f/fmap inc >n0)) ;; not shared
 !! (def >n2      (f/shared (f/fmap inc >n1)))
 !! (def >out (f/cap >n2))
 !! (def >out-local (f/cap >n-local))
 !! (f/with-executor (remote-executor aqueue atrace noop)
      (deliver-trace-element! aqueue atrace [2 (Just 2)])
      (deliver-trace-element! aqueue atrace [1 (Just 1)])
      (deliver-trace-element! aqueue atrace [0 (Just 0)])
      (f/put >n0 0))
 @>out => 2
 @>out-local => 1
 @atrace => {0 (Just 0)
             1 (Just 1)
             2 (Just 2)})

(defn remote-executor [aqueue atrace putf]
  (let [next-id (f/counter)]
    (fn [& [this & args]]
      (if-not (.-shared this)
        (apply f/compute-executor this args)
        (let [id (or (.-id this) (next-id))]
          (set! (.-id this) id)
          (let [trace-value (get @atrace id)]
            (case (f/node-type this)
              "From"            (let [value (first args)];; (put >a value)
                                  (or trace-value
                                      (do (putf value)
                                          (enqueue! aqueue id this)
                                          (Nothing))))
              ("ApplyN" "Bind") (or trace-value
                                    (do (enqueue! aqueue id this)
                                        (Nothing))))))))))

(tests
 ;; Bind returned subgraph is part of the trace.
 !! (def aqueue (atom {}))
 !! (def atrace (atom {}))
 !! (def >p (f/input))
 !! (def >q (f/input))
 !! (def >control (f/input))
 !! (def >cross (f/bindR >control (fn [c] (case c :p >p :q >q))))
 !! (def >z (f/fmap identity >cross))
 !! (def >out (f/cap >z))
 !! (f/with-executor (f/tracing-executor (fn [[id v]] (swap! atrace assoc id v)))
      (f/put >control :p)
      (f/put >p 'p))
 #_(= @atrace {0 (Just :p)
             1 (Just (.-node >p))})
 ;; !! (f/with-executor (remote-executor aqueue atrace noop)
      ;; (f/put >control :p)
 ;;      (f/put >p 'p))
 ;; @>out => 2
 ;; @atrace => {0 (Just 0)
 ;;             1 (Just 1)
 ;;             2 (Just 2)}
 )
