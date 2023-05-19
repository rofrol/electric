(ns wip.button-with-progress-as-value
  (:require
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [missionary.core :as m]))

(e/defn Download! [round]
  (if (< round 5)
    (let [ms (+ 100 (rand-int 500))]
      (case (new (e/task->cp (m/sleep ms ms) ::busy))
        ::busy (* (inc round) 20)
        (recur (inc round))))
    100))

(e/defn App []
  (e/client
    (dom/div (dom/style {:display "flex", :align-items "center", :gap "1rem"})
      (let [progress (dom/button (dom/text "Download")
                       (let [progress (e/do-event [_ (e/listen> dom/node "click")]
                                        (let [progress (e/server (new Download! 0))]
                                          (cond-> progress (= progress 100) reduced)))
                             busy? (not (reduced? progress))]
                         (dom/props {:disabled busy?, :aria-busy busy?})
                         progress))]
        (dom/div (dom/style {:border "1px solid gray", :border-radius "0.4rem"
                             :overflow "hidden", :width "5rem", :height "1rem"})
          (dom/div (dom/style {:width (str (if (reduced? progress)
                                             (case (unreduced progress) ::e/init 0 100)
                                             progress)
                                        "%")
                               :height "100%"
                               :background-color "green"})))))))
