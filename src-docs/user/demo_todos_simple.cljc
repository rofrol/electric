(ns user.demo-todos-simple
  (:import [hyperfiddle.electric Pending])
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil)) ; database on server
(e/def db) ; injected database ref; Electric defs are always dynamic

(e/defn OnTick [id server-checked?]
  (dom/for-each "change"
    (e/fn [e]
      (let [checked? (-> e .-target .-checked)]
        (try (e/server (d/transact! !conn [{:db/id id :task/status (if checked? :done :active)}]) nil)
             (catch Pending _ :keep)
             (catch missionary.Cancelled _)
             (catch :default e (.error js/console e) (set! (.-checked dom/node) server-checked?) nil))))))

(e/defn TodoItem [id]
  (e/server
    (let [e (d/entity db id)
          status (:task/status e)
          server-checked? (= :done status)]
      (e/client
        (dom/div
          (dom/input (dom/props {:type "checkbox"})
            (let [busy (-> (OnTick. id server-checked?) seq boolean)]
              (dom/props {:disabled busy, :aria-busy busy})
              (when-not busy (dom/bind-value server-checked? #(set! (.-checked %) %2))))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

#?(:cljs (defn submit? [e] (and (= "Enter" (.-key e)) (not= "" (-> e .-target .-value)))))
#?(:cljs (defn read-value! [node] (let [v (.-value node)] (set! (.-value node) "") v)))

(e/defn InputSubmit [F]
  ; Custom input control using lower dom interface for Enter handling
  (dom/input
    (dom/for-each "keydown"
      (e/fn [e]
        (when (submit? e)
          (let [v (read-value! dom/node)]
            (try (new F v)
                 (catch Pending _ :keep)
                 (catch missionary.Cancelled _)
                 (catch :default e (.error js/console e)))))))
    (dom/props {:placeholder "Buy milk"})))

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
          (InputSubmit. (e/fn [v] (e/server (d/transact! !conn [{:task/description v :task/status :active}]) nil)))
          (dom/div {:class "todo-items"}
            (e/server
              (e/for-by :db/id [{:keys [db/id]} (todo-records db)]
                (TodoItem. id))))
          (dom/p (dom/props {:class "counter"})
            (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
            (dom/text " items left")))))))
