(ns hyperfiddle.client-server
  (:require [hyperfiddle.fabric :as f :refer [defnode]]))

;; #?(:cljs (f/set-executor! (f/tracing-executor js/console.log)))

(defnode >a (f/input))
(defnode >b (f/fmap inc >a))
(defnode >c (f/on >b (partial prn ">c")))

(defn put-in-a [v]
  (f/put >a v))
