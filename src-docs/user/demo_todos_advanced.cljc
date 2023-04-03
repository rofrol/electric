(ns user.demo-todos-advanced
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil)) ; database on server
(comment (alter-var-root #'!conn (fn [_] (d/create-conn {}))))
(e/def db) ; injected database ref; Electric defs are always dynamic

(defonce !order-id #?(:clj (atom 0) :cljs nil))

#?(:clj (defn tx! [latency tx]
          (m/sp
            (m/? (m/sleep latency))
            (if (even? @!order-id)
              (throw (ex-info "Even" {}))
              (d/transact! !conn tx)))))

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
    (dom/on= "keydown" (fn [e] (when (= "Enter" (.-key e))
                                 (when-some [v (contrib.str/empty->nil (-> e .-target .-value))]
                                   (set! (.-value dom/node) "")
                                   [e v])))
      (e/fn [[_ v]] (new F v)))))

(defn ->task [desc] {:task/description desc, :task/status :active, :task/order (swap! !order-id inc)})

(e/defn TodoCreate [] (InputSubmit. (e/fn [v] (e/server (new Tx! [(->task v)])))))

#?(:clj (defn todo-count [db]
          (count
            (d/q '[:find [?e ...] :in $ ?status
                   :where [?e :task/status ?status]] db :active))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id :task/description :task/order]) ...]
                      :where [?e :task/status]] db)
            (sort-by :task/order))))

(e/defn ConsumeGood [!succeeded] (e/for [[k _] (e/watch !succeeded)] (swap! !succeeded dissoc k)))
(e/defn ReportBad [!failed] (e/for [[k ex] (e/watch !failed)] (.error js/console ex) (swap! !failed dissoc k)))
(e/defn RunDefault [on=] (let [[_ !succeeded !failed] on=] (ConsumeGood. !succeeded) (ReportBad. !failed)))

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
                  (let [[running !succeeded !failed] (TodoCreate.)]
                    (dom/div {:class "todo-items"}
                      (e/server
                        (e/for-by :db/id [{:keys [db/id]} (todo-records db)]
                          (TodoItem. id)))
                      (e/for [[_ v] running] (dom/div (dom/text "⌛ " v)))
                      (ConsumeGood. !succeeded)
                      (e/for [[[_ v :as k] ex] (e/watch !failed)]
                        (.error js/console v ex)
                        (dom/div
                          (dom/text "⚔ " v)
                          (dom/button (dom/text "⟳")
                            (RunDefault. (dom/on= "click" (fn [e] [e v])
                                           (e/fn [_]
                                             (dom/props {:aria-busy true, :disabled true})
                                             (case (e/server (new Tx! [(->task v)]))
                                               (swap! !failed dissoc k))))))))))
                  (dom/p (dom/props {:class "counter"})
                    (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
                    (dom/text " items left")))))))))))
