(ns hyperfiddle.electric-crud
  #?(:cljs (:require-macros hyperfiddle.electric-crud))
  (:require
   [contrib.str]
   [hyperfiddle.electric-dom2 :as dom]))

(defmacro button [V! & body]
  `(dom/button
     (dom/for-each (dom/listen "click") ~V!
       (dom/props {:disabled dom/busy})
       ~@body)))

(defn ?read-value! [e node]
  (when (and (= "Enter" (.-key e)) (contrib.str/blank->nil (.-value node)))
    (let [txt (.-value node)] (set! (.-value node) "") txt)))

(defmacro enter [V! & body]
  `(dom/input
     (dom/for-each (dom/listen "keydown")
       (e/fn [e#]
         (when-some [v# (?read-value! e# dom/node)]
           (new ~V! v#)))
       ~@body)))

(defmacro checkbox [v V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (dom/for-each (dom/listen "change") (e/fn [e_#] (new ~V! (.-checked dom/node)))
       ;; we don't check for focus on purpose
       ;; tested in FF, a ticking with the mouse focuses the node
       ;; in case tx fails we want to revert to value, even if focused
       (when-not dom/busy (set! (.-checked dom/node) ~v))
       ~@body)))

(defmacro input [v V! & body]
  `(dom/input
     (dom/for-each (dom/listen "input") (e/fn [e_#] (new ~V! (.-value dom/node)))
       (when-not (or busy (dom/focused?)) (set! (.-value dom/node) ~v))
       ~@body)))
