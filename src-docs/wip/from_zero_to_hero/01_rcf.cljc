(ns wip.from-zero-to-hero.01-rcf
  (:require [hyperfiddle.rcf :as rcf :refer [tests tap %]]
            [missionary.core :as m]))

;; RCF stands for Rich Comment Forms. Clojure users often write (comment ..)
;; blocks to capture thoughts, tinker with the code or even document behavior.
;; RCF turns this workflow into actual tests! You can start with a `comment`
;; block and turn it into a `tests` block with minimal effort.
;;
;; RCF tests are written in a `tests` block. If testing is turned on, e.g.
;; with `(rcf/enable!)`, it will generate test code, otherwise it behaves like a
;; `comment` block and expands to nothing. This means your test code can live in
;; the same namespace as your implementation and run in dev/test mode but be
;; disabled in production. You can still move the tests in a separate namespace
;; if you wish to.
;;
;; The most common tests test for equality or throwing a type of exception.
;; These cases are covered with a shorthand infix syntax `:=` and `:throws`.
;;
;; Let's write a test for our own `abs` which should return the absolute value:

(defn my-abs [x] (if (pos? x) x (- x)))

;; we can tinker with this in a `comment` block:

(comment
  (my-abs 10)                           ; => 10
  (my-abs -5)                           ; => 5
  )

;; Turning this into a `tests` block is trivial:

(tests
  (my-abs 10) := 10
  (my-abs -5) := 5
  )

;; `a := b` desugars to `(clojure.test/is (= b a))` so the tests are
;; clojure.test compatible! Actually you can use any clojure.test matcher as an
;; infix operator and it will just work! E.g. `match?` from matcher combinators
;; could be used as

(comment
  (assoc {} :a 1) match? {:a int?}
  )

;; Let's see a throwing call

(tests
  (my-abs :foo) :throws #?(:clj Exception :cljs js/Error)
  )

;; As you can see reader conditionals also work just fine.

;; RCF tests have 1 more superpower - `tap` and `%`. They allow writing very
;; clean async tests. `tap` will push a value on a queue and `%` will pop it,
;; waiting up to `rcf/*timeout*` milliseconds.
;;
;; These helpers are, again, built to just work in a `comment` block. `%` does
;; nothing and `tap` prints to the console.

;; Here is a cross-platrofm `setTimeout`-like helper that will run a function
;; after given milliseconds. The implementation is not important to understand.
(defn after [ms thunk] ((m/sp (m/? (m/sleep ms)) (thunk)) identity identity))

;; One can play with it in a `comment` form

(comment
  (after 10 #(tap 10))
  (after 100 #(tap 100))
  (after 20 #(tap 20))
  )

;; Turning this into a `tests` block is, again, trivial:

(tests
  (after 10 #(tap 10))
  (after 100 #(tap 100))
  (after 20 #(tap 20))
  % := 10
  % := 20
  % := 100
  )

;; To wait longer, just `(rcf/set-timeout! 1000)` in the `tests` block.

;; Try to think how would you write this test with your test tool of choice.
;; I'll wait... Got it? Now make it cross-platform! I hope this thought exercise
;; will make you appreciate RCF :)
;;
;; Congratulations! You know everything you need to know about RCF and have some
;; ideas how to use it effectively. Let's move to Electric functions in
;; 02_electric_fns.cljc
