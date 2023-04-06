(ns hyperfiddle.electric-dom2-test
  (:import [hyperfiddle.electric Pending]
           #?(:clj [clojure.lang ExceptionInfo]))
  (:require [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.rcf :as rcf :refer [tests with tap %]]
            [hyperfiddle.electric :as e]
            [missionary.core :as m]))

(tests "dom/*for-each"
  (def !push (atom nil))
  (def !resolvers (atom {}))
  (defn resolve! [k v] (reset! (@!resolvers k) v))
  (with (e/run (tap (try (dom/*for-each (fn [pusher] (reset! !push pusher) #(do))
                           (e/fn [e]
                             (let [!v (atom :pending)]
                               (swap! !resolvers assoc e !v)
                               (try (let [v (e/watch !v)]
                                      (case v
                                        :pending  (throw (Pending.))
                                        :caught   (throw (ex-info "caught" {}))
                                        :uncaught (throw (ex-info "uncaught" {}))
                                        #_else    v))
                                    (catch Pending _ :pending)
                                    (catch #?(:clj Throwable :cljs :default) e
                                      (case (ex-message e)
                                        "caught" nil
                                        #_else   (throw e)))))))
                         (catch #?(:clj Throwable :cljs :default) e [(type e) (ex-message e)]))))
    #_init                   % := {}
    (@!push 0),              % := {0 :pending}
    (@!push 1),              % := {0 :pending, 1 :pending}
    (resolve! 1 nil),        % := {0 :pending}
    (resolve! 0 false),      % := {}
    (@!push 2),              % := {2 :pending}
    (resolve! 2 :caught),    % := {}
    (@!push 99),             % := {99 :pending}
    (resolve! 99 :uncaught), % := [ExceptionInfo "uncaught"]
    (resolve! 99 :alive),    % := {99 :alive}
    (resolve! 99 nil),       % := {}
    (tap ::done), % := ::done, (println " ok")
    )
  )
