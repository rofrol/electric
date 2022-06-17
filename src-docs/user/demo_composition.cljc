(ns user.demo-composition
  (:require [hyperfiddle.photon :as p]
            [hyperfiddle.photon-dom :as dom]
            [hyperfiddle.zero :as z]
            [user.demo-2-system-properties :as sys-props]
            [user.demo-3-webview :as webview]
            [user.demo-5-button :as button]
            [clojure.edn :as edn])
  (:import (hyperfiddle.photon Pending))
  #?(:cljs (:require-macros user.demo-composition)))      ; forces shadow hot reload to also reload JVM at the same time

(p/defn App []
  (dom/div
   (dom/h1 (dom/text "Pick a demo:"))
   (let [selected (dom/select {:value "1"}
                              (dom/option {:value "1"} (dom/text "1 - Button"))
                              (dom/option {:value "2"} (dom/text "2 - System Properties"))
                              (dom/option {:value "3"} (dom/text "3 - Webview"))
                              (dom/events "change" (map (dom/oget "target" "value")) "3"))]
     (case selected
       "1" (button/App.)
       "2" (sys-props/App.)
       "3" (dom/div
            (dom/h1 (dom/text "Webview(s)"))
            (let [count (dom/input {:type :range :min 1, :value 2 :max 4 :step 1, :list "tickmarcks"
                                    :style {:width "10rem"}}
                                   (dom/events "input" (map (dom/oget "target" "value")) 2))]
              (dom/datalist {:id "tickmarcks"}
                            (dom/option {:value 1})
                            (dom/option {:value 2})
                            (dom/option {:value 3})
                            (dom/option {:value 4}))
              count ; hack
              (dom/div {:style {:display :grid
                                :grid-gap "1rem"
                                :grid-template-columns "1fr 1fr"}}
                       (dom/for [_ (range count)]
                         (webview/App.))))
            (dom/h2 (dom/text "Add new info"))
            (dom/div {:style {:display :grid
                              :grid-template-columns "1fr auto"
                              :grid-gap "0.5rem"}}
                     (let [initial-tx "[{:order/email \"dan@example.com\"}]"
                           tx (dom/input {:value initial-tx}
                                         (dom/events "input" (map (dom/oget "target" "value")) initial-tx))]
                       tx ; hack
                       (when (dom/button (dom/text "Transact")
                                         (z/impulse ~@(p/watch webview/conn) (dom/>events "click")))
                         ~@(do (webview/transact! (edn/read-string tx))
                               nil))))))))) 

(def main #?(:cljs (p/client (p/main
                              (try
                                (binding [dom/parent (dom/by-id "root")]
                                  (App.))
                                (catch Pending _))))))

(comment
  #?(:clj (user/browser-main! `main)))
