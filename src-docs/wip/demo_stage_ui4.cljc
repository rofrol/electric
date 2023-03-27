(ns wip.demo-stage-ui4
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]))

;; run under toxiproxy as
;; ./src-dev/toxiproxy.sh 8083 8080 300
;; to have on 8083 the app with 300ms latency
;;
;; input-glitch:
;; - overwrite "name" value by typing e.g. 123<tab> quickly
;; - the input glitches, i.e. it resets to previous values
;;
;; focus-glitch:
;; - overwrite "name" value by typing e.g. 1<tab> quickly
;; - now the second field is focused, yet its value updates

(def !label #?(:clj (atom "cobblestone") :cljs nil))
#?(:cljs (defn target-value [e] (-> e .-target .-value)))
(e/defn Input [] (dom/on "input" (e/fn [e] (let [v (target-value e)] (e/server (reset! !label v))))))

;; input-glitch: no
;; focus-glitch: yes
(e/defn Form1 []
  (let [label (e/watch !label)]
    (e/client
      (dom/dl
        (dom/dt (dom/text "name"))
        (dom/dd (dom/input (Input.) (when-not (dom/Focused?.) (set! (.-value dom/node) label))))
        (dom/dt (dom/text "name"))
        (dom/dd (dom/input (Input.) (when-not (dom/Focused?.) (set! (.-value dom/node) label))))))))

(e/defn Name [label]
  (dom/dt (dom/text "name"))
  (dom/dd (dom/input (Input.) (when-not (dom/Focused?.) (set! (.-value dom/node) label)))))

;; input-glitch: yes
;; focus-glitch: no
;; changes: factored out dom/input to separate e/fn
(e/defn Form2 []
  (let [label (e/watch !label)]
    (e/client
      (dom/dl
        (Name. label)
        (Name. label)))))
