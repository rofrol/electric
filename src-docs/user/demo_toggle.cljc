(ns user.demo-toggle
  (:import [hyperfiddle.electric Pending])
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]))

; a full stack function with both frontend and backend parts,
; all defined in the same expression

#?(:clj (defonce !x (atom true))) ; server state
(e/def x (e/server (e/watch !x))) ; reactive signal derived from atom

(e/defn Toggle []
  (e/client
    (dom/h1 (dom/text "Toggle Client/Server"))

    (dom/div
      (dom/text "number type here is: "
        (case x
          true (e/client (pr-str (type 1))) ; javascript number type
          false (e/server (pr-str (type 1)))))) ; java number type

    (dom/div (dom/text "current site: "
               (case x
                 true "ClojureScript (client)"
                 false "Clojure (server)")))

    (dom/button (dom/text "toggle client/server")
      (let [busy (-> (dom/for-each "click" (e/fn [_] (try (e/server (swap! !x not)) nil (catch Pending _ :keep))))
                   seq boolean)]
        (dom/props {:disabled busy, :aria-busy busy})))))
