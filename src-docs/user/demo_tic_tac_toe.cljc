(ns user.demo-tic-tac-toe
  (:import [hyperfiddle.electric Pending])
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

(def !x #?(:clj (atom (vec (repeat 10 0))) :cljs nil))
(e/def x (e/server (e/watch !x)))
(defn update-board [board pos] (update board pos #(case % 0 1, 1 2, 2 0)))

(e/defn Button [offset]
  (dom/button
    (dom/for-each "click" (e/fn [_] (try (e/server (swap! !x update-board offset))
                                         (catch Pending _ :keep)
                                         (catch missionary.Cancelled _)
                                         (catch :default e (.error js/console e)))))
    (dom/text
      (case (e/server (nth x offset))
        2 "x"
        1 "o"
        0 "-"))))

(e/defn TicTacToe []
  (e/client
    (dom/h1 (dom/text "Tic Tac Toe \uD83C\uDFAE"))
    (dom/p (dom/text "multiplayer works, try two tabs"))
    (dom/table
      (e/for [row [[0 1 2]
                   [3 4 5]
                   [6 7 8]]]
        (dom/tr
          (e/for [i row]
            (dom/td
              (Button. i))))))))
