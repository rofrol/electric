(ns hyperfiddle.client-server
  (:require [hyperfiddle.fabric :as f :refer [defnode]]))

;; #?(:cljs (f/set-executor! (f/tracing-executor js/console.log)))

(defnode >a (f/input))
(defnode >b (f/fmap inc >a))
#?(:cljs (defnode >c (f/on >b (constantly nil))))

(defn put-in-a [v]
  (f/put >a v))
