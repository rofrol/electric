(ns user.demo-todos-simple
  (:require #?(:clj [datomic.api :as d]) ; database on server
            #?(:clj [contrib.datomic :as cd])
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]))

#?(:clj
   (def schema
     [{:db/ident :task/status,      :db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
      {:db/ident :task/description, :db/valueType :db.type/string,  :db/cardinality :db.cardinality/one}]))

(def db-uri "datomic:mem://todos-simple")
;; to clear DB data replace temporarily both with `def`
#?(:clj (defonce !conn (do (d/delete-database db-uri)
                           (d/create-database db-uri)
                           (doto (d/connect db-uri) (d/transact schema)))))
#?(:clj (defonce <db (->> (m/ap
                            (m/amb
                              (d/db !conn)
                              (:db-after (m/?> (cd/tx-report-queue> !conn)))))
                       (m/relieve {}) m/signal)))

(e/def db) ; injected database ref; Electric defs are always dynamic

(e/defn TodoItem [id]
  (e/server
    ;; we'd use `d/entity` is not for this Datomic bug
    ;; https://ask.datomic.com/index.php/859/equality-on-d-entity-ignores-db?show=859#q859
    (let [{:task/keys [status description]} (d/pull db '[:task/status :task/description] id)]
      (e/client
        (prn :render id)
        (dom/div
          (ui/checkbox
            (case status :active false, :done true)
            (e/fn [v]
              (e/server
                (e/discard
                  (e/offload-latest d/transact !conn [{:db/id id, :task/status (if v :done :active)}]))))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text description)))))))

#?(:clj (defn todo-count [db]
          (count
            (d/q '[:find [?e ...] :in $ ?status
                   :where [?e :task/status ?status]] db :active))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id :task/description]) ...]
                      :where [?e :task/status]] db)
            (sort-by :task/description))))

(e/defn TodoList []
  (e/server
    (binding [db (new (identity <db))]
      (e/client
        (dom/h1 (dom/text "minimal todo list"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (dom/div (dom/props {:class "todo-list"})
          (dom/input (dom/props {:placeholder "Buy milk"})
            (ui/on-submit (e/fn [v] (e/server (e/offload-latest d/transact !conn [{:task/description v, :task/status :active}]) nil))))
          (dom/div {:class "todo-items"}
            (e/server
              (e/for-by :db/id [{:keys [db/id]} (e/offload-latest todo-records db)]
                (TodoItem. id))))
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (e/offload-latest todo-count db))))
            (dom/text " items left")))))))
