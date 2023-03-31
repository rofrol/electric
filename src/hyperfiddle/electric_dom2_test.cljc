(ns hyperfiddle.electric-dom2-test
  (:require [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
            [hyperfiddle.electric :as e]
            [missionary.core :as m]))

(defn- q [] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))

(tests "dom/*on="
  (def !x (atom nil))
  (def !resolvers (atom (q)))
  (defn qpop! [aq] (-> (swap-vals! aq pop) first peek))
  (defn resolve! [f] (let [dfv (qpop! !resolvers)] (dfv f)))
  (with (e/run (let [[running succeeded failed] (dom/*on= (e/watch !x) (e/fn [x]
                                                                         (let [dfv (m/dfv)]
                                                                           (swap! !resolvers conj dfv)
                                                                           ((new (e/task->cp dfv)) x))))]
                 (tap [:running running])
                 (tap [:succeeded succeeded])
                 (tap [:failed failed])))

    ;; note that !x starts with nil but nils are skipped, otherwise we'd see [:running #{nil}]
    #_init                                   (hash-set % % %) := #{[:running #{}] [:succeeded {}] [:failed {}]}
    (reset! !x 0)                                          %  := [:running #{0}]
    (resolve! inc)                             (hash-set % %) := #{[:running #{}] [:succeeded {0 1}]}
    (swap! !x inc)                                         %  := [:running #{1}]
    (resolve! #(throw (ex-info "boo" {:v %}))) (hash-set % %) := #{[:running #{}] [:failed {1 _}]}

    (tap ::done), % := ::done, (println " ok")))

(tests "dom/*on= succeeded and failed contain only last new values"
  (def !x (atom nil))
  (def !resolvers (atom (q)))
  (defn qpop! [aq] (-> (swap-vals! aq pop) first peek))
  (defn resolve! [f] (let [dfv (qpop! !resolvers)] (dfv f)))
  (with (e/run (let [[_ succeeded failed] (dom/*on= (e/watch !x) (e/fn [x]
                                                                   (let [dfv (m/dfv)]
                                                                     (swap! !resolvers conj dfv)
                                                                     ((new (e/task->cp dfv)) x))))]
                 (tap [:succeeded succeeded])
                 (tap [:failed failed])))
    (hash-set % %) := #{[:succeeded {}] [:failed {}]}
    (reset! !x 0), (resolve! inc), % := [:succeeded {0 1}]
    (reset! !x 1), (resolve! inc), % := [:succeeded {1 2}]
    (reset! !x 2), (resolve! #(throw (ex-info "boo" {:v %}))), % := [:failed {2 _}]
    (reset! !x 3), (resolve! #(throw (ex-info "boo" {:v %}))), % := [:failed {3 _}]

    (tap ::done), % := ::done, (println " ok"))
  )

(tests "relieve with reset! on atom watch"
  (def !x (atom {}))
  (def it ((m/relieve merge (m/watch !x)) #(do) #(do)))
  @it := {}
  (reset! !x {:a 1})
  @it := {:a 1}
  (reset! !x {:b 2})
  (reset! !x {:c 3})
  @it := {:b 2, :c 3}

  (tap ::done), % := ::done, (println " ok")
  )
