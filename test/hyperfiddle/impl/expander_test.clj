(ns hyperfiddle.impl.expander-test
  (:require [hyperfiddle.electric.impl.expander :as expander]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [contrib.debug :as dbg]
            [hyperfiddle.electric :as e])
  )

(e/def X 1)
(e/defn Foo [x] (-> x inc))

(tests
  (expander/expand nil) := nil
  (expander/expand 1) := 1
  (expander/expand '(inc 1)) := '(inc 1)
  (expander/expand '[(-> 1 inc)]) := '[(inc 1)]
  (expander/expand '{(-> 1 inc) (-> 1 inc)}) := '{(inc 1) (inc 1)}
  (expander/expand (seq '(-> 1 inc))) := '(inc 1)

  (expander/expand '(let [x 1] x)) := '(let* [x 1] x)
  (expander/expand '(let [x (let [y 1] y)] x)) := '(let* [x (let* [y 1] y)] x)

  (expander/expand '(do 1 2)) := '(do 1 2)
  (expander/expand '(do (let [x 1] x) (let [y 2] y))) := '(do (let* [x 1] x) (let* [y 2] y))

  (expander/expand '(inc (let [x 1] x))) := '(inc (let* [x 1] x))

  (expander/expand '(let [with-open inc] (with-open 1))) := '(let* [with-open inc] (with-open 1))
  (expander/expand '(let [with-open inc, x (with-open inc)] x)) := '(let* [with-open inc, x (with-open inc)] x)

  (expander/expand '(case (-> 1 inc) (2) (-> 2 inc) (with-open) 3 4)) := '(case (inc 1) (2) (inc 2) (with-open) 3 4)

  (expander/expand ''(-> 1 inc)) := (quote (-> 1 inc))

  (expander/expand '(fn [x] 1)) := '(fn* ([x] 1))
  (expander/expand '(fn foo [x] 1)) := '(fn* foo ([x] 1))
  (expander/expand '(fn foo ([x] 1))) := '(fn* foo ([x] 1))
  (expander/expand '(fn [with-open] (with-open 1))) := '(fn* ([with-open] (with-open 1)))
  (expander/expand '(fn [x] (-> x inc))) := '(fn* ([x] (inc x)))

  (expander/expand '(letfn [(foo [with-open] (with-open 1)) ; don't expander/expand with-open
                            (bar [x] (-> x inc))            ; expander/expand ->
                            (baz [x] (->> x)) ; don't expander/expand ->>, it is shadowed in letfn scope
                            (->> [x] x)]
                      (-> (->> x) inc)))

  := '(letfn* [foo (fn* foo ([with-open] (with-open 1)))
               bar (fn* bar ([x] (inc x)))
               baz (fn* baz ([x] (->> x)))
               ->> (fn* ->> ([x] x))]
        (inc (->> x)))

  (expander/expand '(set! (.-x (-> [(java.awt.Point. (-> 0 inc) 2)] first)) (-> 2 inc)))
  := '(set! (. (first [(new java.awt.Point (inc 0) 2)]) -x) (inc 2))

  (expander/expand '(new java.awt.Point (-> 0 inc) 1)) := '(new java.awt.Point (inc 0) 1)
  (expander/expand '(java.awt.Point. (-> 0 inc) 1)) := '(new java.awt.Point (inc 0) 1)
  (expander/expand '(new (missionary.core/seed [(-> 0 inc)]))) := '(new (missionary.core/seed [(inc 0)]))

  (expander/expand '(try (-> 1 inc)
                         (catch Throwable with-open (with-open 1))
                         (finally (-> 0 dec))))
  := '(try (inc 1)
           (catch Throwable with-open (with-open 1))
           (finally (dec 0)))

  (expander/expand '(catch (-> 1 inc))) := '(catch (inc 1))

  (expander/expand '(loop [with-open inc, x 2] (-> x with-open)))
  := '(loop* [with-open inc, x 2] (with-open x))

  (expander/expand 'X) := 1
  ;; (expander/expand '(new Foo 1))

  (println " ok")
  )
