(ns user.tutorial-7guis-5-crud
  #?(:cljs (:require-macros [user.tutorial-7guis-5-crud]))
  (:import [hyperfiddle.electric Pending])
  (:require [clojure.string :as str]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [missionary.core :as m]))

;;; Instructions
;; https://eugenkiss.github.io/7guis/tasks#crud

#?(:cljs (defn tx! [latency f] (m/sp (m/? (m/sleep latency)) (f))))
(e/def latency)
(e/defn Tx! [tx] (new (e/task->cp (tx! latency tx))) nil)

;;; App state

(def !state (atom {:selected nil
                   :stage {:name ""
                           :surname ""}
                   :names (sorted-map 0 {:name "Emil", :surname "Hans"})}))

;;; Business logic

(def next-id (partial swap! (atom 0) inc))

(defn select! [id]
  (swap! !state (fn [state] (assoc state :selected id, :stage (get-in state [:names id])))))

(defn set-name! [name]
  (swap! !state assoc-in [:stage :name] name))

(defn set-surname! [surname]
  (swap! !state assoc-in [:stage :surname] surname))

(defn create! [] (swap! !state (fn [{:keys [stage] :as state}]
                                 (-> state
                                   (update :names assoc (next-id) stage)
                                   (assoc :stage {:name "", :surname ""})))))
(defn delete! [] (swap! !state (fn [{:keys [selected] :as state}]
                                 (-> state (update :names dissoc selected) (dissoc :selected)))))
(defn update! [] (swap! !state (fn [{:keys [selected stage] :as state}]
                                 (assoc-in state [:names selected] stage))))

(defn filter-names [names-map needle]
  (if (empty? needle)
    names-map
    (let [needle (str/lower-case needle)]
      (reduce-kv (fn [r k {:keys [name surname]}]
                   (if (or (str/includes? (str/lower-case name) needle)
                         (str/includes? (str/lower-case surname) needle))
                     r
                     (dissoc r k)))
        names-map names-map))))

;;; Presentation

(defmacro ->busy [& body] `(try (do ~@body) false (catch Pending _ true)))

(e/defn Button [label callback disabled?]
  (dom/button (dom/text label)
    (let [busy (->busy (dom/on-bp "click" (e/fn [_] (Tx!. callback))))]
      (dom/props {:disabled (or disabled? busy), :aria-busy busy}))))

(e/defn Latency [min max init]
  (dom/div (dom/style {:display "flex", :flex-direction "column"})
    (let [lat (dom/input (dom/props {:type "range" :min min, :max max, :value init, :style {:width "200px"}})
                (parse-long (dom/Value.)))]
      (dom/div (dom/text "Latency: " lat "ms")
        (dom/style {:order "-1"}))
      lat)))

(e/defn CRUD []
  (e/client
    (dom/h1 (dom/text "7 GUIs: CRUD"))
    (let [state (e/watch !state)
          selected (:selected state)]
      (binding [latency (Latency. 0 2000 200)]
        (dom/div (dom/props {:style {:display :grid
                                     :grid-gap "0.5rem"
                                     :align-items :baseline
                                     :grid-template-areas "'a b c c'\n
                                                         'd d e f'\n
                                                         'd d g h'\n
                                                         'd d i i'\n
                                                         'j j j j'"}})
          (dom/span (dom/props {:style {:grid-area "a"}})
            (dom/text "Filter prefix:"))
          (let [!needle (atom ""), needle (e/watch !needle)]
            (ui4/input needle (e/fn [v] (reset! !needle v))
              (dom/props {:style {:grid-area "b"}}))
            (dom/ul (dom/props {:style {:grid-area "d"
                                        :background-color :white
                                        :list-style-type :none
                                        :padding 0
                                        :border "1px gray solid"
                                        :height "100%"}})
              (e/for [entry (filter-names (:names state) needle)]
                (let [id (key entry)
                      value (val entry)]
                  (dom/li (dom/text (:surname value) ", " (:name value))
                    (dom/props {:style {:cursor :pointer
                                        :color (if (= selected id) :white :inherit)
                                        :background-color (if (= selected id) :blue :inherit)
                                        :padding "0.1rem 0.5rem"}})
                    (dom/on "click" (e/fn [_] (select! id))))))))
          (let [stage (:stage state)]
            (dom/span (dom/props {:style {:grid-area "e"}}) (dom/text "Name:"))
            (ui4/input (:name stage) (e/fn [v] (set-name! v))
              (dom/props {:style {:grid-area "f"}}))
            (dom/span (dom/props {:style {:grid-area "g"}}) (dom/text "Surname:"))
            (ui4/input (:surname stage) (e/fn [v] (set-surname! v))
              (dom/props {:style {:grid-area "h"}})))
          (dom/div (dom/props {:style {:grid-area "j"
                                       :display :grid
                                       :grid-gap "0.5rem"
                                       :grid-template-columns "auto auto auto 1fr"}})
            (Button. "Create" create! false)
            (Button. "Update" update! (not selected))
            (Button. "Delete" delete! (not selected))))))))
