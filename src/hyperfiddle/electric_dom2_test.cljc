(ns hyperfiddle.electric-dom2-test
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.rcf :as rcf :refer [% with tap tests]])
  (:import [hyperfiddle.electric Pending]))

;; flaky, the events are racing
;; dom/bind-value without Unglitch usually fails the test
;; dom/bind-value with Unglitch usually passes the test
(tests "input glitch"
  (def !label (atom ""))
  (def !focused? (atom true))
  (def !typed (atom ""))
  (with (e/run
          (try
            (let [label (e/watch !label), focused? (e/watch !focused?), typed (e/watch !typed)]
              (tap [:focused? focused?])
              (dom/on dom/EventImpulse typed
                (e/fn [e] (let [txt (identity e)] (e/server (e/offload #(reset! !label txt))))))
              (dom/bind-value label #(tap [:set! %]) focused?))
            (catch Pending _)
            (catch #?(:clj Throwable :cljs :default) e (prn [(type e) (ex-message e)]))))
    % := [:focused? true]
    (reset! !typed "a")
    (reset! !typed "ab")
    (reset! !typed "abc")
    (reset! !focused? false)
    % := [:focused? false]
    % := [:set! "abc"]
    (tap :done) % := :done)
  )
