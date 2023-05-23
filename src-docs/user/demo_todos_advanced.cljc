(ns user.demo-todos-advanced
  (:import [hyperfiddle.electric Pending]
           [missionary Cancelled])
  (:require #?(:clj [datomic.api :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.electric-ui4 :as ui]
            [hyperfiddle.electric-ui5 :as ui5]
            [contrib.debug :as dbg]
            [hyperfiddle.electric.impl.runtime :as r]))

#?(:clj (defn init-conn []
          (let [uri "datomic:mem://db"]
            (d/delete-database uri)
            (d/create-database uri)
            (d/connect uri))))

(defonce !conn #?(:clj (init-conn) :cljs nil)) ; database on server
#?(:clj (comment (alter-var-root #'!conn (fn [_] (d/connect "datomic:mem://db")))))
(e/def db) ; injected database ref; Electric defs are always dynamic

#?(:clj
   (def schema
     [{:db/ident :task/status,      :db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
      {:db/ident :task/description, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one}
      {:db/ident :hf/stable-id,     :db/valueType :db.type/uuid,   :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}]))

(defonce !db #?(:clj (atom nil) :cljs nil))
(defonce !taker #?(:clj (future
                          (reset! !db (d/db !conn))
                          (let [q (d/tx-report-queue !conn)]
                            (loop []
                              (reset! !db (:db-after (.take ^java.util.concurrent.LinkedBlockingQueue q)))
                              (recur))))
                   :cljs nil))

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
              (d/transact !conn (dbg/dbg :tx tx))))))

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

(defn tempid? [x] (string? x))

(e/defn ReadEntity [id]
  (try
    (e/server [::e/init (into {} (d/touch (d/entity db id)))])
    (catch Pending _ [::e/pending nil])
    (catch :default e [::e/failed e])))

(e/defn CreateEntity [id record]
  (try ; create is never ::e/init
    (e/server ; return txns to stage, not inline txn
      (new Tx! [record]) ; returns tx-report which has :ids->tempids
      [::e/ok (into {} (d/touch (d/entity db id)))])
    (catch Pending _ [::e/pending record]) ; optimistic
    (catch :default e [::e/failed e])))

(e/defn EnsureEntity [id record]
  (if-not (tempid? id)
    (ReadEntity. id)
    (CreateEntity. id record))) ; todo must be idempotent

(e/defn TodoItemOld [record]
  (e/client
    (dom/div
      (ui5/checkbox
        record
        (e/fn [v] (= :done (:task/status v)))
        (e/fn [checked?]
          (e/server (new Tx! [[:db/add (:db/id record)
                               :task/status (if checked? :done :active)]])))
        EnsureEntity
        (dom/props {:id e}))
      (dom/label (dom/props {:for e}) (dom/text (e/server (:task/description e)))))))

(e/defn MasterList [TodoItem]
  (e/server
    ; create new (modal semantics, user submits via "stage" or "enter", can be rapid) 
    (let [_ (ui5/modal (TodoItem. {:task/status :active}))
          optimistic-records .
          txns hf/stage]
      
      (e/for-by :hf/stable-id [record (todo-records db)] optimistic-records
        (TodoItem. record)))))

(e/defn TodoItem [record]
  (e/client
    (dom/div (dom/pre (dom/text record))
      #_(ui5/entity record EnsureEntity) 
      (ui5/checkbox)
      (ui5/input))))

(e/defn AdvancedTodoList []
  (e/server
    (binding [db (e/watch !db), Tx! (e/fn [tx] (new (e/task->cp (tx! tx))) nil)]
      (d/transact !conn schema)
      (prn :db db)
      (e/client
        (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (Latency. 0 2000)
        (FailRate. 0 10)
        (dom/div (dom/props {:class "todo-list"})
          ;(dom/div {:class "todo-items"}) 
          
          (MasterList. TodoItem)
                    
          (let [optimistic-records
                (dom/input (dom/props {:placeholder "Buy milk"}) ; todo move into TodoItem
                  (->> (m/observe (fn [!] (e/dom-listener dom/node "keydown" #(some-> (ui/?read-line! dom/node %) !))))
                    (m/eduction (map (fn [input-val]
                                       {:hf/stable-id (random-uuid)
                                        :task/description input-val
                                        :task/status :active
                                        #_#_:task/order (e/server (swap! !order-id inc))})))
                    (m/reductions conj [])
                    new))]
            (prn optimistic-records)

            (e/client
              (e/for-by-optimistic :hf/stable-id [record (e/offload #(todo-records db))] ; syntax bad
                optimistic-records
                (TodoItem. record)))

            (e/client
              (e/for-event :hf/stable-id [record (mx/mix
                                                   ; e/server here is broken, can't move >xs
                                                   (e/server (e/diff> :hf/stable-id (e/offload #(todo-records db))))
                                                   (e/client (e/diff> :hf/stable-id optimistic-records)))]
                (TodoItem. record)))

            (e/client #_e/server ; fixme
              (e/for-by :hf/stable-id [record (into #{} cat [(e/server (todo-records db)) optimistic-records])]
                (TodoItem. record)))
            
            #_(e/server
                (e/for-by :hf/stable-id [record (todo-records db)] optimistic-records
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
