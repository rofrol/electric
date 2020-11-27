(ns hyperfiddle.trace
  (:require [hyperfiddle.fabric :as f]
            [hyperfiddle.hxclj :as hx]
            [minitest :refer [tests]])
  (:import [hyperfiddle Maybe View]))

(defn- node-type [node]
  (keyword (aget hyperfiddle.NodeDef/__hx_constructs (.. node -def -index))))

(defn- node-data [node]
  {:type   (node-type node)
   :ended  (.-ended node)
   :frame  (.-frame node)
   :id     (.-id node)
   :name   (.-name node)
   :queued (.-queued node)
   :rank   (.-rank node)
   :val    (.-val node)
   :ok     (.ok node)})

(defn datafy
  "Given a Dataflow View node (like `(.-node >v)`), return a map description.
  Recursively walks upward (towards inputs)."
  ([node]
   (datafy false node))
  ([recursive? node]
   (when node
     (cond-> (node-data node)
       (and recursive? (.-on node)) (assoc :on (mapv (partial datafy true) (hx/hx->clj (.-on node))))))))

(def nodes (partial tree-seq :on :on))

(defn- unwrap [val]
  (condp = (type val)
    Maybe (some-> (.-params ^Maybe val) (aget 0))))

(defn snapshot [^View view]
  (->> (.-node view)
       (datafy true)
       (nodes)
       (filter :name)
       (map (fn [{:keys [name val]}]
              [name (unwrap val)]))
       (reverse)))

(tests
 !! (do
      (require '[hyperfiddle.fabric :as f :refer [defnode]])
      (require '[hyperfiddle.viz :as viz])
      (def trace (atom {}))
      (defnode >control (f/input))
      (defnode >a (f/input))
      (defnode >b (f/input))
      (defnode >cross (f/bindR >control (fn [x] (case x :a >a :b >b))))
      (defnode >out (f/on >cross (partial println "=> ")))
      (f/set-executor! (f/tracing-executor (fn [[name v]]
                                             (do (prn [name v])
                                                 (swap! trace (fn [trace]
                                                                (cond
                                                                  (isa? (type v) View) (if-let [v' (get trace (.-name (.-node v)))]
                                                                                         (assoc trace name v')
                                                                                         (assoc trace name v))
                                                                  :else                (assoc trace name v))))))))
      (swap! trace empty)
      (viz/capture-gif "/tmp/out" >out
                       (f/put >control :a)
                       (f/put >a 1)
                       (f/put >control :b)
                       (f/put >b 2))
      )
 !! (do
      (f/set-executor! (f/cache-or-compute-executor #(deref trace)))
      (f/put >control :b)
      (f/put >b 100)))


;; lazy sampling
