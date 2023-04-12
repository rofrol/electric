(ns user.tutorial-7guis-4-timer
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

;; https://eugenkiss.github.io/7guis/tasks#timer

(defn seconds [milliseconds] (/ (Math/floor (/ milliseconds 100)) 10))

(defn second-precision [milliseconds] (-> milliseconds (/ 1000) (Math/floor) (* 1000))) ; drop milliseconds

(defn now [] #?(:cljs (second-precision (js/Date.now))))

(e/defn Timer []
  (e/client
    (dom/h1 (dom/text "7 GUIs: Timer"))
    (dom/div (dom/props {:style {:display :grid, :width "20em", :grid-gap "0 1rem", :align-items :center}})
      (let [goal (dom/input (dom/props {:type "range", :min 0, :max 60, :value 10})
                   (dom/style {:grid-row 3, :grid-column 2})
                   (parse-long (dom/Value.)))
            goal-ms (* 1000 goal)
            !start (atom (now)), start (e/watch !start)
            time (min goal-ms (- (second-precision e/system-time-ms) start))]
        (dom/span (dom/props {:style {:grid-row 3}}) (dom/text "Duration: " goal "s"))
        (dom/span (dom/text "Elapsed Time:"))
        (dom/progress (dom/props {:max goal-ms, :value time, :style {:grid-column 2}}))
        (dom/span (dom/text (seconds time) " s"))
        (dom/button (dom/text "Reset")
          (dom/style {:grid-row 4, :grid-column "1/3"})
          (dom/on! "click" (fn [_] (reset! !start (now)))))))))
