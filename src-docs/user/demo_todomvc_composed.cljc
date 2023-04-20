(ns user.demo-todomvc-composed
  (:require
    #?(:clj [datascript.core :as d])
    [hyperfiddle.electric :as e]
    [hyperfiddle.electric-dom2 :as dom]
    [user.demo-todomvc :as todomvc]))

(def !n #?(:clj (atom 1)))

(e/defn PopoverCascaded [i F]
  (dom/div (dom/style {:position "absolute", :width "50vw"
                       :left (str (* i 40) "px"), :top (str (-> i (* 40) (+ 60)) "px")
                       :z-index (+ i (if (dom/hovered?) 1000 0))})
    (F.)))

(e/defn TodoMVC-composed []
  (e/client
    (let [state (e/watch todomvc/!state)
          n (e/server (e/watch !n))]
      (e/server
        (binding [todomvc/db (e/watch todomvc/!conn)
                  todomvc/transact! (partial d/transact! todomvc/!conn)]
          (e/client
            (dom/link (dom/props {:rel :stylesheet, :href "/todomvc.css"}))
            (dom/input (dom/props {:type "range", :min 1, :max 25, :step 1})
              (let [n (parse-long (dom/->value))]
                (e/server (reset! !n n))))
            (dom/div (dom/props {:class "todomvc" :style {:position "relative"}})
              (dom/h1 (dom/text "TodoMVC"))

              (e/for [i (range n)] ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                (PopoverCascaded. i
                  (e/fn [] (todomvc/TodoMVC-UI. state)))))))))))
