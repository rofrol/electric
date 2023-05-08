(ns user.demo-chat
  (:import [hyperfiddle.electric Pending])
  (:require [contrib.data :refer [pad]]
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-crud :as crud]))

#?(:clj (defonce !msgs (atom (list))))
(e/def msgs (e/server (reverse (pad 10 nil (e/watch !msgs)))))

(e/defn Chat []
  (e/client
    (try
      (dom/h1 (dom/text "Multiplayer chat app in 30 LOC"))
      (dom/p (dom/text "try two tabs!"))
      (let [!in (atom nil), in (e/watch !in)]
        (dom/ul (dom/style {:padding-left "1.5em"})
          (e/server
            (e/for-by identity [msg msgs]
              (e/client
                (dom/li (dom/style {:visibility (if (nil? msg) "hidden" "visible")})
                  (dom/text msg)))))
          (crud/enter in (e/fn [v]
                           (let [!ex (atom false), ex (e/watch !ex)]
                             (if ex
                               (dom/li (dom/text "💀 " v)
                                 (crud/button (e/fn [] (reset! !ex false))
                                   (dom/text "↻"))
                                 (prn [(type ex) (ex-message ex)])
                                 (throw (Pending.)))
                               (try (e/server (if (zero? (rand-int 2))
                                                (swap! !msgs #(cons v (take 9 %)))
                                                (throw (ex-info "bad luck" {}))))
                                    (catch Pending e (dom/li (dom/text "⌛ " v)) (throw e))
                                    (catch :default e (reset! !ex e) (throw (Pending.))))))))
          ;; possible factoring
          #_(crud/enter in
            ;; submit
            (e/fn [v] (e/server (if (zero? (rand-int 2))
                                  (swap! !msgs #(cons v (take 9 %)))
                                  (throw (ex-info "bad luck" {})))))
            ;; pending
            (e/fn [v] (dom/li (dom/text "⌛ " v)))
            ;; error
            (e/fn [v ex retry!]
              (dom/li (dom/text "💀 " v)
                (crud/button (e/fn [] (retry!)) (dom/text "↻"))
                (prn [(type ex) (ex-message ex)]))))
          )
        (dom/input (dom/props {:placeholder "Type a message"}) (reset! !in dom/node))
        nil)
      (catch Pending e
        (dom/style {:background-color "yellow"})))))

; A chat app. Open it in two tabs. When you type a message, both tabs update.
; This works because both tabs share a single JVM which means they subscribe to the same atom.
