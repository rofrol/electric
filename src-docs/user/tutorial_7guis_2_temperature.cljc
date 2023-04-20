(ns user.tutorial-7guis-2-temperature
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [missionary.core :as m]
   [clojure.math :as math]))

;; https://eugenkiss.github.io/7guis/tasks#temp

(defn c->f [c] (+ (* c (/ 9 5)) 32))
(defn f->c [f] (* (- f 32) (/ 5 9)))
(defn random-value [_] (m/sp (m/? (m/sleep 2000)) (rand-int 250)))

(e/defn TemperatureConverter []
  (e/client
    (dom/h1 (dom/text "Temperature Converter"))
    (let [!t (atom 0), t (e/watch !t)]
      ;; turn on to see concurrent updates
      #_(reset! !t (new (e/task->cp (random-value t))))
      (dom/dl
        (dom/dt (dom/text "Celsius"))
        (dom/dd
          (dom/input (dom/props {:type "number"})
            (dom/on! "input" (fn [e] (when-some [v (-> e .-target .-value parse-long)] (reset! !t v))))
            (set! (.-value dom/node) (math/round t))))
        (dom/dt (dom/text "Farenheit"))
        (dom/dd
          (dom/input (dom/props {:type "number"})
            (dom/on! "input" (fn [e] (when-some [v (-> e .-target .-value parse-long)] (reset! !t (f->c v)) nil)))
            (set! (.-value dom/node) (math/round (c->f t)))))))))
