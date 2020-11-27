(ns hyperfiddle.native-trace
  (:require [cognitect.transit :refer [write-handler]]
            [hyperfiddle.hxclj :as hx :refer [hx->clj]]
            [hyperfiddle.transit :as t]
            [hyperfiddle.fabric :as f])
  (:import hyperfiddle.Push
           hyperfiddle.NodeDef
           hyperfiddle.Maybe))

(require '[hyperfiddle.fabric :as f :refer [defnode]])

(defn- node-type [node]
  (keyword (aget NodeDef/__hx_constructs (.. node -def -index))))

(defn- maybe-type [m]
  (keyword (aget Maybe/__hx_constructs (.. m -index))))

(defn traverse [^Push node]
  (some-> node (.-on) hx->clj seq))

(def write-handlers
  {Push  (write-handler (constantly "Node") (fn [^Push v] [(node-type v) (.-id v) (.-name v) (.-val v) (map #(.-id %) (traverse v))]))
   Maybe (write-handler (constantly "Maybe") (fn [^Maybe v] (let [type (maybe-type v)]
                                                             (case type
                                                               :Just    [:Just (aget (.-params v) 0)]
                                                               :Nothing [:Nothing]))))})

(def nodes (partial tree-seq traverse traverse))

(defn serialize [^Push node]
  (t/encode node :type :json :opts {:handlers write-handlers}))


(do
  (defnode >a (f/input))
  (defnode >b (f/fmap inc >a))
  (defnode >c (f/on >b prn)))
(f/put >a 1)

(->> (.-node >c)
     (nodes)
     (reverse)
     (map serialize))
;; =>
;; Tag       Type        ID    Name    Value                     Ref to parents
( ["~#Node", ["~:From",  220,  "~$>a", ["~#Maybe",["~:Just",1]], ["~#list",[]]]]
  ["~#Node", ["~:ApplyN",221,  "~$>b", ["~#Maybe",["~:Just",2]], ["~#list",[220]]]]
  ["~#Node", ["~:Into",  222,  "~$>c", ["~#Maybe",["~:Just",2]], ["~#list",[221]]]])

;; ↑ This is transit encoded

