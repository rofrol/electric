(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require [hyperfiddle.electric-dom2 :as dom]))

(defmacro handle [getter V!]
  `(e/fn [e#]
     (dom/style {:background-color "yellow"})
     (when-some [v# (~getter e#)] (new ~V! v#))))

#?(:cljs (defn value [e] (-> e .-target .-value)))
#?(:cljs (defn checked [e] (-> e .-target .-checked)))

;; TODO
;; - Pending
;; - exceptions
(defmacro input [v V! & body]
  `(dom/input
     (when (empty? (dom/for-each "input" (handle value ~V!)))
       (dom/bind-value ~v))
     ~@body))
