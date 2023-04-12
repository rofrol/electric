(ns user.demo-chat
  (:import [hyperfiddle.electric Pending])
  (:require [contrib.data :refer [pad]]
            [contrib.str :refer [empty->nil]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

#?(:clj (defonce !msgs (atom (list))))
(e/def msgs (e/server (reverse (pad 10 nil (e/watch !msgs)))))

#?(:cljs (defn submit? [e] (and (= "Enter" (.-key e)) (not= "" (-> e .-target .-value)))))
#?(:cljs (defn read-value! [node] (let [v (.-value node)] (set! (.-value node) "") v)))

(e/defn Chat []
  (e/client
    (try
      (dom/h1 (dom/text "Multiplayer chat app in 30 LOC"))
      (dom/p (dom/text "try two tabs!"))
      (dom/ul (dom/style {:padding-left "1.5em"})
        (e/server
          (e/for-by identity [msg msgs]
            (e/client
              (dom/li (dom/style {:visibility (if (nil? msg) "hidden" "visible")})
                (dom/text msg))))))
      (dom/input
        (dom/for-each "keydown"
          (e/fn [e]
            (when (submit? e)
              (let [v (read-value! dom/node)]
                (try (e/server (swap! !msgs #(cons v (take 9 %)))) nil
                     (catch Pending _ :keep)
                     (catch :default e (.error js/console e)))))))
        (dom/props {:placeholder "Type a message"}))
      (catch Pending e
        (dom/style {:background-color "yellow"})))))

; A chat app. Open it in two tabs. When you type a message, both tabs update.
; This works because both tabs share a single JVM which means they subscribe to the same atom.
