(ns wip.demo-stage-ui4
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

;; run under toxiproxy as
;; ./src-dev/toxiproxy.sh 8083 8080 300
;; to have on 8083 the app with 300ms latency

(def !label #?(:clj (atom "cobblestone") :cljs nil))
#?(:cljs (defn target-value [e] (-> e .-target .-value)))
(e/defn Input [] (dom/on "input" (e/fn [e] (let [v (target-value e)]  (e/server (reset! !label v))))))

#_(dom/input (Input.)
    (when-some [l (when-not (dom/Focused?.) label)]
      (set! (.-value dom/node) l)))

;; typing 12<tab> in the input-glitch field will
;; - glitch the input-glitch field, old value will show up
;; - glitch the focus-glitch field, it will receive new values while focused
(e/defn Form []
  (let [label (e/watch !label)]
    (e/client
      (dom/dl
        (dom/dt (dom/text "input-glitch"))
        (dom/dd (new (e/fn [label]
                       (dom/input (Input.) (when-not (dom/Focused?.)
                                             (case (e/Unglitch. label)
                                               (set! (.-value dom/node) label)))))
                  label))
        (dom/dt (dom/text "focus-glitch"))
        (dom/dd (dom/input (Input.)
                  (when-some [l (when-not (dom/Focused?.) label)]
                    (set! (.-value dom/node) l))))))))
