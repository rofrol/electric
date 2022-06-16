(ns user.demo-6-todos-basic
  (:require clojure.edn
            [datascript.core :as d]
            [missionary.core :as m]
            [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z])
  (:import [hyperfiddle.photon Pending])
  #?(:cljs (:require-macros user.demo-6-todos-basic)))

(def auto-inc (partial swap! (atom 0) inc))

(defn task-status [id status]
  [{:db/id       id
    :task/status status}])

(defn task-remove [id])                                     ; todo

(def !conn #?(:clj (d/create-conn {})))

(comment
  (d/transact !conn (task-create "buy milk"))
  (d/q '[:find ?s . :in $ ?e :where [?e :task/status ?s]] @!conn 1)
  := :active)

(defn clear-input! [el v] (dom/set-properties! el {:value ""}) v)

(p/def db)                                                  ; server

(defn task-create [description]
  [{:db/id            (str "dustin-" (auto-inc))
    :task/description description
    :task/status      :active}])

(p/defn Todo-list [basis-t]
  (dom/div
    (dom/h1 (dom/text "Todo list - basic"))
    (concat
      (dom/input {:type "text"}
             (->> (dom/>keychord-events #{"enter"})
             (m/eduction
               (map (comp task-create (dom/oget :target :value)))
               (map (partial clear-input! dom/parent)))
             (z/impulse basis-t)))
      (dom/div
        (apply concat
               (dom/for [id ~@(d/q '[:find [?e ...] :in $ :where [?e :task/status]] db)]
                 (dom/div
                   (concat
                     (dom/input {:type "checkbox"
                                 :checked (#{:done} ~@(:task/status (d/entity db id)))}
                       (->> (dom/>events "input"
                                         (comp
                                          (map (comp {false :active true :done} (dom/oget :target :checked)))
                                          (map (partial task-status id))))
                            (z/impulse basis-t)))
                     (dom/span (dom/text (str ~@(:task/description (d/entity db id))))))))))
      (dom/p
        (dom/text (str ~@(count (d/q '[:find [?e ...] :in $ ?status
                                       :where [?e :task/status ?status]]
                                     db :active)) " items left"))))))

(defn transact [tx-data] #?(:clj (do (prn `transact tx-data) (d/transact! !conn tx-data) nil)))

(p/defn App []
  (let [!t      (atom 0)
        basis-t (p/watch !t)]
    (prn `basis-t basis-t)
    ~@(binding [db (p/watch !conn)]
        ~@(if-some [tx (seq (Todo-list. basis-t))]
            (swap! !t + (do ~@(transact tx) 1))             ; auto-transact
            (prn :idle)))))

(def main #?(:cljs (p/client (p/main (try (binding [dom/parent (dom/by-id "root")]
                                            (App.))
                                          (catch Pending _))))))

(comment
  (user/browser-main! `main)
  )