(ns contrib.assert
  #?(:cljs (:require-macros contrib.assert))
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn- error! [pred-expr v-expr v]
  (let [msg (str "check failed: (" (pr-str pred-expr) " " (pr-str v-expr) ") for " (pr-str v))]
    (throw (ex-info msg {})))) ; todo elide top frames

(defn -check [pred-expr pred v-expr v]
  (cond
    (keyword? pred) (when-not (or (= pred v) ; special rule - keyword equality
                                (pred v))
                      (error! pred-expr v-expr v))
    () (when-not (pred v)
         (error! pred-expr v-expr v)))
  v)

(defmacro check
  ([v] `(check some? ~v))
  ([pred v] `(-check '~pred ~pred '~v ~v)))

(tests
  (check nil) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 2) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check odd? 1) := 1
  (check ::done ::done) := ::done
  (check ::done ::not-done) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check ::done 42) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
  (check ::done nil) :throws #?(:clj clojure.lang.ExceptionInfo :cljs js/Error))