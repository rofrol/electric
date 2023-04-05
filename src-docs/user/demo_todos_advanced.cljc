(ns user.demo-todos-advanced
  (:import [hyperfiddle.electric Pending]
           [missionary Cancelled])
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [contrib.debug :as dbg]
            [missionary.core :as m]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil)) ; database on server
(comment (alter-var-root #'!conn (fn [_] (d/create-conn {}))))
(e/def db) ; injected database ref; Electric defs are always dynamic

(defonce !order-id #?(:clj (atom 0) :cljs nil))

#?(:clj (def !latency (atom 200)))
#?(:clj (def !fail-rate (atom 1)))
#?(:clj (defn tx! [tx]
          (m/sp
            (m/? (m/sleep @!latency))
            (if (< (rand-int 10) @!fail-rate)
              (throw (ex-info "Even" {}))
              (d/transact! !conn tx)))))

(e/def Tx!)

(e/defn Latency [min max init]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (let [lat (dom/input (dom/props {:type "range" :min min, :max max, :value init, :style {:width "200px"}})
                (parse-long (dom/Value.)))]
      (dom/span (dom/text "Latency: " lat "ms")
        (dom/style {:order "-1"}))
      lat)))

(e/defn FailRate [min max init]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (let [rate (dom/input (dom/props {:type "range", :min min, :max max, :value init, :style {:width "200px"}})
                 (parse-long (dom/Value.)))]
      (dom/span (dom/text "Fail Rate: " rate " out of " max)
        (dom/style {:order -1}))
      rate)))

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

(defn ->task [desc] {:task/description desc, :task/status :active, :task/order (swap! !order-id inc)})

#?(:clj (defn todo-count [db]
          (count
            (d/q '[:find [?e ...] :in $ ?status
                   :where [?e :task/status ?status]] db :active))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id :task/description :task/order]) ...]
                      :where [?e :task/status]] db)
            (sort-by :task/order))))

#?(:cljs (defn submit? [e] (and (= "Enter" (.-key e)) (not= "" (-> e .-target .-value)) e)))
#?(:cljs (defn read-value! [node] (let [v (.-value node)] (set! (.-value node) "") v)))

(e/defn Failed [v]
  (dom/div (dom/text "💀 " v)
    (dom/button (dom/text "⟳")
      (-> (dom/for-each "click"
            (e/fn [_]
              (try (e/server (new Tx! [(->task v)])) ::done
                   (catch Pending _ (dom/props {:aria-busy true, :disabled true}) :keep)
                   (catch Cancelled _ (prn :inner-cancelled))
                   (catch :default e (.error js/console "retry" v e)))))
        vals first #{::done} not))))

(defmacro snapshot [form] `(new (m/eduction (take 1) (e/fn [] ~form))))

(e/defn TodoList []
  (e/server
    (binding [db (e/watch !conn)]
      (e/client
        (dom/h1 (dom/text "minimal todo list"))
        (dom/p (dom/text "it's multiplayer, try two tabs"))
        (e/server
          (reset! !latency (e/client (Latency. 0 2000 200)))
          (reset! !fail-rate (e/client (FailRate. 0 10 1)))
          (binding [Tx! (e/fn [tx] (new (e/task->cp (tx! tx))) nil)]
            (e/client
              (dom/div (dom/props {:class "todo-list"})
                (let [in (dom/input (dom/props {:placeholder "Buy milk"}) dom/node)]
                  (dom/div {:class "todo-items"}
                    (e/server
                      (e/for-by :db/id [{:keys [db/id]} (todo-records db)]
                        (TodoItem. id)))
                    (dom/for-each in "keydown"
                      (e/fn [e]
                        (when (submit? e)
                          (let [v (read-value! in)]
                            (try (e/server (new Tx! [(->task v)]))
                                 (catch Pending _ (dom/div (dom/text "⌛ " v)) :keep)
                                 (catch Cancelled _ (prn :cancelled?))
                                 (catch :default e (.error js/console v e) (Failed. v)))))))))
                (dom/p (dom/props {:class "counter"})
                  (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
                  (dom/text " items left"))))))))))
