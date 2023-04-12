(ns user.tutorial-7guis-1-counter
  (:require
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]))

;; https://eugenkiss.github.io/7guis/tasks#counter

(e/defn Counter []
  (e/client
    (let [!state (atom 0)]
      (dom/p (dom/text (e/watch !state)))
      (dom/button (dom/text "Count")
        (dom/on! "click" (fn [_] (swap! !state inc)))))))
