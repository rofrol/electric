(ns user.demo-todos-advanced
  (:import [hyperfiddle.electric Pending]
           [missionary Cancelled])
  (:require #?(:clj [datascript.core :as d]) ; database on server
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [hyperfiddle.electric-crud :as crud]))

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
              (throw (ex-info "tx failed" {:tx tx}))
              (d/transact! !conn tx)))))

(e/def Tx!)

(e/defn Latency [min max init]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (let [lat (dom/input (dom/props {:type "range" :min min, :max max, :value init, :style {:width "200px"}})
                (parse-long (dom/->value)))]
      (dom/span (dom/text "Latency: " lat "ms") (dom/style {:order "-1"}))
      lat)))

(e/defn FailRate [min max init]
  (dom/span (dom/style {:display "inline-flex", :flex-direction "column"})
    (let [rate (dom/input (dom/props {:type "range", :min min, :max max, :value init, :style {:width "200px"}})
                 (parse-long (dom/->value)))]
      (dom/span (dom/text "Fail Rate: " rate " out of " max) (dom/style {:order -1}))
      rate)))

(defn ->task [desc] {:task/description desc, :task/status :active, :task/order (swap! !order-id inc)})

#?(:clj (defn todo-count [db] (count (d/q '[:find [?e ...] :where [?e :task/status :active]] db))))

#?(:clj (defn todo-records [db]
          (->> (d/q '[:find [(pull ?e [:db/id :task/description :task/order]) ...] :where [?e :task/status]] db)
            (sort-by :task/order))))

(e/defn TodoItem [id]
  (e/server
    (let [e (d/entity db id)
          status (:task/status e)
          server-checked? (= :done status)]
      (e/client
        (dom/div
          (crud/checkbox server-checked?
            (e/fn [checked?]
              (e/server (new Tx! [{:db/id id :task/status (if checked? :done :active)}])))
            (dom/props {:id id}))
          (dom/label (dom/props {:for id}) (dom/text (e/server (:task/description e)))))))))

(e/defn Failed [v]
  (dom/div (dom/text "💀 " v)
    (dom/button (dom/text "⟳")
      ;; currently broken due to Cancelled in userspace bug
      ;; https://www.notion.so/hyperfiddle/no-Cancelled-in-userspace-3e516dba8cbe4ca082be3ac9879808d5?pvs=4
      (-> (e/for-event [_ (dom/listen dom/node "click")]
            (try (e/server (new Tx! [(->task v)])) ::done
                 (catch Pending _ (dom/props {:aria-busy true, :disabled true}))
                 (catch Cancelled _ (prn :inner-cancelled))
                 (catch :default e (.error js/console "retry" v e))))
        vals first #{::done} not))))

(e/defn AdvancedTodoList []
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
                    (e/for-event [e (dom/listen in "keydown")]
                      (when-some [v (crud/?read-line! e in)]
                        (try (e/server (new Tx! [(->task v)]))
                             (catch Pending _ (dom/div (dom/text "⌛ " v)) true)
                             (catch Cancelled _ (prn :cancelled))
                             (catch :default e (e/EventExceptionHandler e v) (Failed. v)))))))
                (dom/p (dom/props {:class "counter"})
                  (dom/span (dom/props {:class "count"}) (dom/text (e/server (todo-count db))))
                  (dom/text " items left"))))))))))
