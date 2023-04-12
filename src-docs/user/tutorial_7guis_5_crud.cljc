(ns user.tutorial-7guis-5-crud
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [clojure.string :as str]))

;;; Instructions
;; https://eugenkiss.github.io/7guis/tasks#crud

;;; App state

(def !state (atom {:selected nil
                   :stage {:name "", :surname ""}
                   :names (sorted-map 0 {:name "Emil", :surname "Hans"})}))

;;; Business logic

(def next-id (partial swap! (atom 0) inc))

(defn select! [id] (swap! !state (fn [state] (assoc state :selected id, :stage (get-in state [:names id])))))
(defn set-name! [name] (swap! !state assoc-in [:stage :name] name))
(defn set-surname! [surname] (swap! !state assoc-in [:stage :surname] surname))

(defn create! [] (swap! !state (fn [{:keys [stage] :as state}]
                                 (-> state
                                   (update :names assoc (next-id) stage)
                                   (assoc :stage {:name "", :surname ""})))))
(defn update! [] (swap! !state (fn [{:keys [selected stage] :as state}]
                                 (assoc-in state [:names selected] stage))))
(defn delete! [] (swap! !state (fn [{:keys [selected] :as state}]
                                 (-> state (update :names dissoc selected) (dissoc :selected)))))

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

(e/defn CRUD []
  (e/client
    (dom/h1 (dom/text "7 GUIs: CRUD"))
    (let [state (e/watch !state)
          selected (:selected state)]
      (dom/div (dom/style {:display :grid, :grid-gap "0.5rem", :align-items :baseline
                           :grid-template-areas "'a b c c'\n
                                                 'd d e f'\n
                                                 'd d g h'\n
                                                 'd d i i'\n
                                                 'j j j j'"})
        (dom/span (dom/style {:grid-area "a"})
          (dom/text "Filter prefix:"))
        (let [needle (dom/input (dom/style {:grid-area "b"}) (dom/Value.))]
          (dom/ul (dom/style {:grid-area "d", :background-color :white, :list-style-type :none
                              :padding 0, :border "1px gray solid", :height "100%"})
            (e/for [entry (filter-names (:names state) needle)]
              (let [id (key entry), value (val entry)]
                (dom/li (dom/text (:surname value) ", " (:name value))
                  (dom/style {:cursor :pointer, :padding "0.1rem 0.5rem"
                              :color (if (= selected id) :white :inherit)
                              :background-color (if (= selected id) :blue :inherit)})
                  (dom/on! "click" (fn [_] (select! id))))))))
        (let [stage (:stage state)]
          (dom/span (dom/style {:grid-area "e"}) (dom/text "Name:"))
          (dom/input
            (let [busy (-> (dom/for-each "input" (e/fn [e] (set-name! (-> e .-target .-value)) nil)) seq boolean)]
              (when-not busy (dom/bind-value (:name stage)))
              (dom/props {:style {:grid-area "f"}, :disabled busy, :aria-busy busy})))
          (dom/span (dom/style {:grid-area "g"}) (dom/text "Surname:"))
          (dom/input
            (let [busy (-> (dom/for-each "input" (e/fn [e] (set-surname! (-> e .-target .-value)) nil)) seq boolean)]
              (when-not busy (dom/bind-value (:surname stage)))
              (dom/props {:style {:grid-area "h"}, :disabled busy, :aria-busy busy}))))
        (dom/div (dom/style {:grid-area "j", :display :grid, :grid-gap "0.5rem"
                             :grid-template-columns "auto auto auto 1fr"})
          (dom/button (dom/text "Create") (dom/on! "click" (fn [_] (create!))))
          (dom/button (dom/text "Update") (dom/on! "click" (fn [_] (update!)))
            (dom/props {:disabled (not selected)}))
          (dom/button (dom/text "Delete") (dom/on! "click" (fn [_] (delete!)))
            (dom/props {:disabled (not selected)})))))))
