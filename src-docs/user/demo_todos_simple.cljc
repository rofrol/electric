(ns user.demo-todos-simple
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil)) ; database on server
(e/def db) ; injected database ref; Electric defs are always dynamic

#?(:clj (defn tx! [latency tx] (m/sp (m/? (m/sleep latency)) (d/transact! !conn tx))))
(e/def Tx!)

(e/defn Latency [min max init]
  (dom/div (dom/style {:display "flex", :flex-direction "column"})
    (let [lat (dom/input (dom/props {:type "range" :min min, :max max, :value init, :style {:width "200px"}})
                (parse-long (dom/Value.)))]
      (dom/div (dom/text "Latency: " lat "ms")
        (dom/style {:order "-1"}))
      lat)))

(e/defn TodoItem [id]
  (e/server
    (let [e (d/entity db id)
          status (:task/status e)]
      (e/client
        (dom/div
          (ui/checkbox
            (case status :active false, :done true)
            (e/fn [v]
              (e/server
                (new Tx! [{:db/id id :task/status (if v :done :active)}])))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

(e/defn InputSubmit [F]
  ; Custom input control using lower dom interface for Enter handling
  (dom/input (dom/props {:placeholder "Buy milk"})
    (dom/on-cc "keydown" (e/fn [e]
                           (when (= "Enter" (.-key e))
                             (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                               (new F v)
                               (set! (.-value dom/node) "")))))))

(e/defn TodoCreate []
  (e/client
    (InputSubmit. (e/fn [v]
                    (e/server
                      (new Tx! [{:task/description v :task/status :active}]))))))

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
    (binding [db (e/watch !conn)]
      (e/client
        (dom/h1 (dom/text "minimal todo list"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (let [latency (Latency. 0 2000 200)]
          (e/server
            (binding [Tx! (e/fn [tx] (new (e/task->cp (tx! (doto (rand-int latency) (prn :latency)) tx))) nil)]
              (e/client
                (dom/div (dom/props {:class "todo-list"})
                  (TodoCreate.)
                  (dom/div {:class "todo-items"}
                    (e/server
                      (e/for-by :db/id [{:keys [db/id]} (todo-records db)]
                        (TodoItem. id))))
                  (dom/p (dom/props {:class "counter"})
                    (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
                    (dom/text " items left")))))))))))
