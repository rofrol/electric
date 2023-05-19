(ns user.demo-todos-advanced
  (:import [hyperfiddle.electric Pending]
           [missionary Cancelled])
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.electric-ui5 :as ui5]
            [contrib.debug :as dbg]
            [hyperfiddle.electric.impl.runtime :as r]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil)) ; database on server
(comment (alter-var-root #'!conn (fn [_] (d/create-conn {}))))
(e/def db) ; injected database ref; Electric defs are always dynamic

;; auto-incrementing task id, to define ordering
;; an optimistically rendered task won't jump on the screen
(defonce !order-id #?(:clj (atom 0) :cljs nil))

;; user configurable latency and tx fail rate
#?(:clj (def !latency (atom 200)))
(e/def latency (e/server (e/watch !latency)))

#?(:clj (def !fail-rate (atom 1)))
(e/def fail-rate (e/server (e/watch !fail-rate)))

;; tx with configured latency and fail rate
#?(:clj (defn tx! [tx]
          (m/sp
            (m/? (m/sleep @!latency))
            (if (< (rand-int 10) @!fail-rate)
              (throw (ex-info "tx failed" {:tx tx}))
              (d/transact! !conn tx)))))

(e/def Tx!)

(e/defn Latency [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Latency: " latency "ms"))
    (ui/range latency (e/fn [v] (e/server (reset! !latency v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

(e/defn FailRate [min max]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (dom/span (dom/text "Fail Rate: " fail-rate " out of " max))
    (ui/range fail-rate (e/fn [v] (e/server (reset! !fail-rate v)))
      (dom/props {:min min, :max max, :style {:width "200px"}}))))

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :where [?e :task/status :active]] db))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id
                                       :task/description
                                       :task/status
                                       #_:task/order]) ...]
                      :where [?e :task/status]] db)
            (sort-by :task/order #(compare %2 %1)))))

; render the state of the UI for this stable-id

(e/defn TodoItem [record]
  (e/client
    (dom/div
      (ui5/checkbox
        record
        (e/fn [v] (= :done (:task/status v)))
        (e/fn [checked?]
          (e/server (new Tx! [[:db/add (:db/id record)
                               :task/status (if checked? :done :active)]])))
        (dom/props {:id e}))
      (dom/label (dom/props {:for e}) (dom/text (e/server (:task/description e)))))))

(e/defn MasterList []
  )

(e/defn AdvancedTodoList []
  (e/server
    (binding [db (e/watch !conn), Tx! (e/fn [tx] (new (e/task->cp (tx! tx))) nil)]
      (e/client
        (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (Latency. 0 2000)
        (FailRate. 0 10)
        (dom/div (dom/props {:class "todo-list"})
          ;(dom/div {:class "todo-items"})
          (let [optimistic-records
                (dom/input (dom/props {:placeholder "Buy milk"}) ; todo move into TodoItem
                  (->> (m/observe (fn [!]
                                    (e/dom-listener dom/node "keydown"
                                      #(when-some [v (ui/?read-line! dom/node %)]
                                         (! v)))))
                    (m/eduction (map (fn [input-val]
                                       {:db/id (random-uuid)
                                        :task/description input-val
                                        :task/status :active
                                        #_#_:task/order (e/server (swap! !order-id inc))})))
                    (m/reductions conj [])
                    new))]
            (e/client #_e/server ; fixme
              (e/for-by :db/id [record (clojure.set/union
                                         (set (e/server (todo-records db))) ; will accumulate duplicates (which the union will clear but it's weird)
                                         (set optimistic-records))]
                (TodoItem. record))))
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
            (dom/text " items left")))))))

;; alternative syntax for optimistic rendering
(comment
  (render-optimistically
    :ok (e/fn [v] (e/server (new Tx! [(->task v)])))
    :pending (e/fn [v] (dom/div (dom/text "⌛ " v)))
    :failed (e/fn [v ex retry!]
              (.error js/console ex)
              (dom/div
                (dom/text "💀 " v)
                (ui/button (e/fn [] (retry!))
                  (dom/text "⟳"))))))

;; gotchas
;; - in failed state moving a range picker floods more errors in the console
;; - checkbox failure doesn't revert, is that correct/good?
