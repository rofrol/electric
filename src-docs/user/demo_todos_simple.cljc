(ns user.demo-todos-simple
  #?(:cljs (:require-macros user.demo-todos-simple))
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-crud :as crud]
            [contrib.debug :as dbg]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil)) ; database on server
#?(:clj (defn tx! [tx] (d/transact! !conn (dbg/dbg tx))))
(e/def db) ; injected database ref; Electric defs are always dynamic

(e/defn TodoItem [id]
  (e/server
    (let [e (d/entity db id)
          status (:task/status e)]
      (e/client
        (dom/div
          (crud/checkbox (= :done status)
            (e/fn [checked?]
              (e/server
                ;; can run more than once
                ;; after transaction succeeds we get a new `db`
                ;; the offloaded `todo-records` query returns `Pending`
                ;; this turns `id` to `Pending`, which turns the tx-data `Pending`
                ;; later `id` will resolve to its previous value, causing a new tx to run
                ;; this loop can happen multiple times due to latency
                #_(e/offload #(tx! [{:db/id id :task/status (if checked? :done :active)}]))
                ;; the fix is to `e/snapshot` the transaction function
                ;; subsequent `Pending`s and values are not observed anymore, ensuring run-once
                (if (zero? (rand-int 2))
                  (throw (ex-info "HA!" {}))
                  (e/offload (e/snapshot #(tx! [{:db/id id :task/status (if checked? :done :active)}]))))
                nil))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :where [?e :task/status :active]] db))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id :task/description]) ...] :where [?e :task/status]] db)
            (sort-by :task/description))))

(e/defn TodoList []
  (e/server
    (binding [db (e/watch !conn)]
      (e/client
        (dom/h1 (dom/text "minimal todo list"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (dom/div (dom/props {:class "todo-list"})
          (crud/enter (dom/input dom/node) (e/fn [v] (e/server (e/offload #(d/transact! !conn [{:task/description v :task/status :active}])) nil))
            (dom/props {:placeholder "Buy milk"}))
          (dom/div {:class "todo-items"}
            (e/server
              (e/for-by :db/id [{:keys [db/id]} (e/offload #(todo-records db))]
                (TodoItem. id))))
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
            (dom/text " items left")))))))
