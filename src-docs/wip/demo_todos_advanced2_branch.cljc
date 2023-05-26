(ns wip.demo-todos-advanced2-branch
  (:require
   #?(:clj [datomic.api :as d]) ; database on server
   [contrib.data]
   [contrib.debug :as dbg]
   contrib.str
   [hyperfiddle.branch-tx-test :as hb]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.electric-ui4 :as ui]
   [hyperfiddle.electric-ui5 :as ui5]
   [missionary.core :as m])
  (:import
   [hyperfiddle.electric Pending]))

;; showcases how to render an item optimistically and without e/for-event
;; missing:
;; - ordering
;; - idempotent entity creation
;; - integration of a local and remote seq in e/for-by

#?(:clj
   (def schema
     [{:db/ident :task/status,      :db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
      {:db/ident :task/description, :db/valueType :db.type/string,  :db/cardinality :db.cardinality/one}
      {:db/ident :hf/stable-id,     :db/valueType :db.type/uuid,    :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}]))

#?(:clj (defn init-conn []
          (let [uri "datomic:mem://db"]
            (d/delete-database uri)
            (d/create-database uri)
            (let [conn (d/connect uri)]
              (d/transact conn schema)
              conn))))

(defonce !conn #?(:clj (init-conn) :cljs nil)) ; database on server
#?(:clj (comment (alter-var-root #'!conn (fn [_] (init-conn)))))
(e/def db)

; injected database ref; Electric defs are always dynamic
(defonce !db #?(:clj (atom nil) :cljs nil))
;; singleton database queue polling
;; in the future this can be done with `m/signal`
(defonce !taker #?(:clj (future
                          (reset! !db (d/db !conn))
                          (let [q (d/tx-report-queue !conn)]
                            (loop []
                              (reset! !db (:db-after (.take ^java.util.concurrent.LinkedBlockingQueue q)))
                              (recur))))
                   :cljs nil))

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :where [?e :task/status :active]] db))))

#?(:clj (defn todo-records [db]
          (d/q '[:find [(pull ?e [:db/id :task/description :task/status :hf/stable-id]) ...]
                 :where [?e :task/status]] db)))

(def tempid? (some-fn nil? string?))

(e/defn ReadEntity [id]
  (try
    (e/server [::e/init (into {} (d/touch (d/entity db id)))])
    (catch Pending _ [::e/pending nil])
    (catch :default e [::e/failed e])))

(e/def Stage!)

(e/defn CreateEntity [id record]
  (try ; create is never ::e/init
    (e/server
      (when-not (d/entity db id)
        (let [[state v] (new Stage! [record] (random-uuid))]
          (case state                ; returns tx-report which has :ids->tempids
            ::e/ok [::e/ok (into {} (d/touch (d/entity db id)))]
            ::e/pending [::e/pending record]
            ::e/failed (prn v)))))
    (catch Pending _ [::e/pending record]) ; optimistic
    (catch :default e [::e/failed e])))

(e/defn EnsureEntity [id record]
  (if-not (tempid? id)
    (ReadEntity. id)
    (CreateEntity. id record)))

#?(:cljs (defn ?enter [e]
           (let [line (-> e .-currentTarget .-value)]
             (when (and (= "Enter" (.-key e)) (contrib.str/blank->nil line))
               line))))

(e/defn TodoItem [record]
  (e/client
    (dom/div (dom/style {:display "flex", :align-items "center"})
      (ui5/entity record EnsureEntity
        (let [!edit? (atom false), edit? (e/watch !edit?), id (:hf/stable-id record)]
          (ui5/checkbox (= :done (:task/status record))
            (e/fn [checked?] (new Stage! [[:db/add (:db/id record) :task/status (if checked? :done :active)]] [id :checked]))
            (when edit? (dom/props {:disabled true}))
            (dom/style {:padding "1rem"}))
          (if edit?
            (dom/input
              (let [[state v] (ui5/control "keydown" ?enter identity
                                (:task/description record)
                                (e/fn [v]
                                  (case (new Stage! [[:db/add (:db/id record) :task/description v]] [id :desc])
                                    (reset! !edit? false)))
                                dom/set-val)]
                (new (m/observe (fn [!] (! nil)
                                  (e/dom-listener dom/node "keydown"
                                    #(when (= "Escape" (.-key %)) (reset! !edit? false))))))
                (new (m/observe (fn [!] (! nil)
                                  (e/dom-listener dom/node "blur" #(reset! !edit? false)))))
                (when (= ::e/failed state) (.error js/console v))
                (.focus dom/node)))
            (dom/div (dom/text (:task/description record))
              (new (m/observe (fn [!] (! nil) (e/dom-listener dom/node "dblclick" #(reset! !edit? true))))))))))))

(e/defn AdvancedTodoList []
  (e/server
    (let [global-db (e/watch !db)
          [!branch branch] (hb/Branch!. global-db !conn)]
      (run! prn (conj (:txs branch) '----))
      (binding [db (:db branch)]
        (binding [Stage! (hb/->stage-fn !branch)]
          (e/client
            (binding [Stage! (hb/->stage-fn !branch)]
              (dom/h1 (dom/text "advanced todo list with optimistic render and fail/retry"))
              (dom/p (dom/text "it's multiplayer, try two tabs"))
              (dom/div (dom/props {:class "todo-list"})
                                        ;(dom/div {:class "todo-items"})
                (let [optimistic-records
                      (dom/input (dom/props {:placeholder "Buy milk"}) ; todo move into TodoItem
                        (->> (m/observe (fn [!] (e/dom-listener dom/node "keydown" #(some-> (ui/?read-line! dom/node %) !) {})))
                          (m/eduction (map (fn [input-val]
                                             (let [id (random-uuid)]
                                               {:hf/stable-id id
                                                :task/description input-val
                                                :task/status :active}))))
                          (m/reductions conj [])
                          new))]
                  (e/client #_e/server ; fixme
                    ;; we have a local and remote list of records
                    ;; we'd need to diff them on their respective peers and integrate them on the client
                    ;; currently we don't have this functionality so we hack it
                    ;; by sending the whole server collection to the client
                    (e/for-by :hf/stable-id [record (vals (reduce (fn [ac nx] (assoc ac (:hf/stable-id nx) nx))
                                                            (contrib.data/index-by :hf/stable-id optimistic-records)
                                                            (e/server (todo-records db))))]
                      (TodoItem. record))))
                (dom/p (dom/props {:class "counter"})
                  (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
                  (dom/text " items left"))))))))))
