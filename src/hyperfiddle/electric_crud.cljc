(ns hyperfiddle.electric-crud
  (:refer-clojure :exclude [long double keyword symbol uuid range type])
  #?(:cljs (:require-macros hyperfiddle.electric-crud))
  (:require [contrib.str]
            [clojure.edn]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-svg :as svg]
            [hyperfiddle.electric :as e]
            [missionary.core :as m]
            [contrib.debug :as dbg]))

(defmacro always [& body]
  (let [self (gensym)]
    `(fn ~self
       ([] (do ~@body))
       ([a#] (~self))
       ([a# b#] (~self))
       ([a# b# & more#] (~self)))))

(defn parse-edn [s] (try (some-> s contrib.str/blank->nil clojure.edn/read-string) (catch #?(:clj Throwable :cljs :default) _)))
(defn keep-if [pred v] (when (pred v) v))
(defn parse-keyword [s] (keep-if keyword? (parse-edn s)))
(defn parse-symbol [s] (keep-if symbol? (parse-edn s)))
(defn parse-date [s]
  (try #?(:clj (java.time.LocalDate/parse s) :cljs (js/Date. s))
       (catch #?(:clj Throwable :cljs :default) _)))
(defn parse-datetime-local [s]
  (try #?(:clj (java.time.LocalDateTime/parse s) :cljs (js/Date. s))
       (catch #?(:clj Throwable :cljs :default) _)))

(defmacro each [flow V! & body]
  `(e/each-event ~flow ~V!
     (dom/props {:aria-busy e/busy, :style {:background-color (when e/busy "#fcb467")}})
     ~@body))

#_(defmacro after
  ([ms form] `(case (new (e/task->cp (m/sleep ~ms))) ~form))
  ([ms pending form] `(case (new (e/task->cp (m/sleep ~ms ::done) ~pending)) ::done ~form nil)))

(defmacro keep-for
  ([ms form] `(when (new (e/task->cp (m/sleep ~ms false) true)) ~form))
  ([ms then form] `(case (new (e/task->cp (m/sleep ~ms ::done) ::wait)) ::wait ~form ::done ~then)))

(defmacro click [V!]
  `(let [!state# (atom [::idle]), state# (e/watch !state#)
         pending# (e/for-event [checked?# (dom/listen "click")]
                    (try (let [ret# (new ~V! checked?#)] (case ret# (do (reset! !state# [::ok ret#]) false)))
                         (catch hyperfiddle.electric.Pending _ true)
                         (catch missionary.Cancelled _)
                         (catch :default ex# (reset! !state# [::failed ex#]) false)))]
     (if (seq pending#) [::pending] state#)))

(defmacro button [V! & body]
  `(dom/button
     (let [ret# (do ~@body)
           [state# v#] (click (e/fn [v#] (dom/props {:disabled true, :aria-busy true}) (new ~V! v#)))]
       (case state#
         ::idle    nil
         ::pending (dom/>>style :background-color "yellow")
         ::ok      (keep-for 1000 (dom/>>style :border-color "green"))
         ;; TODO tooltip v# on hover or something smarter than `prn`
         ::failed  (do (prn ::button :err v#) (keep-for 1000 (dom/>>style :border-color "red"))))
       ret#)))

(defn ?read-line! [e node]
  (when (and (= "Enter" (.-key e)) (contrib.str/blank->nil (.-value node)))
    (let [txt (.-value node)] (set! (.-value node) "") txt)))

(defmacro enter
  ([V!]       `(enter dom/node ~V!))
  ([input V!] `(let [inp#  ~input
                     busy# (seq (e/for-event [line# (dom/listen inp# "keydown" #(?read-line! % inp#))]
                                  (try (new ~V! line#) false
                                       (catch hyperfiddle.electric.Pending ex# true))))]
                 (dom/props {:aria-busy busy#, :style {:background-color (when busy# "#fcb467")}})
                 (and busy# (throw (hyperfiddle.electric.Pending.))))))

(defmacro tick [V!]
  `(let [!state# (atom [::init]), state# (e/watch !state#)
         pending# (e/for-event [checked?# (dom/listen dom/node "change" (always (-> dom/node .-checked)))]
                    (try (let [ret# (new ~V! checked?#)] (case ret# (do (reset! !state# [::ok ret#]) false)))
                         (catch hyperfiddle.electric.Pending _ true)
                         (catch missionary.Cancelled _)
                         (catch :default ex# (reset! !state# [::failed ex#]) false)))]
     (if (seq pending#) [::pending] (e/watch !state#))))

(defmacro checkbox [v V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (let [cv# ~v, V# ~V!, ret# (do ~@body), [status# v#] (tick V#)]
       ;; we don't check for focus on purpose
       ;; tested in FF, a ticking with the mouse focuses the node
       ;; in case tx fails we want to revert to value, even if focused
       (case status# ::pending nil (set! (.-checked dom/node) cv#))
       (let [border# (case status#
                       ::ok     (keep-for 1000 nil "3px solid green")
                       ::failed (keep-for 1000 nil "3px solid red")
                       #_else   nil)]
         (dom/props {:aria-busy (= ::pending status#), :style {:outline border#}}))
       (case status# ::failed (prn ::checkbox :err v#) nil)
       ret#)))

(defmacro pending? [v]
  `(try ~v false
        (catch hyperfiddle.electric.Pending _ true)
        (catch ~(if (:env &env) :default 'Throwable) _ false)))

;; TODO is this better?
;; (depend [v#] .-value dom/node)
(defmacro depend [deps form]
  (if (list? form)
    (let [args (vec (repeatedly (+ (dec (count form)) (count deps)) gensym))]
      `((fn ~args (~(first form) ~@(take (dec (count form)) args)))
        ~@(rest form) ~@deps))
    (let [args (vec (repeatedly (count deps) gensym))]
      `((fn ~args ~form) ~@deps))))

(defmacro type [v V!]
  `(let [v# ~v, v-pending?# (pending? v#)
         id# (random-uuid)
         !state# (atom [::init]), state# (e/watch !state#)
         pending# (e/for-event [text# (dom/listen dom/node "input" (always (-> dom/node .-value)))]
                    (try (let [ret# (new ~V! text#)] (case ret# (do (reset! !state# [::ok ret#]) false)))
                         (catch hyperfiddle.electric.Pending _ true)
                         (catch missionary.Cancelled _)
                         (catch :default ex# (reset! !state# [::err ex#]) false)))]
     (cond (or (seq pending#) v-pending?#) [::pending]

           (and (#{::init ::ok} (first state#)) (not v-pending?#))
           (if (dom/focused?)
             (if (= (str v#) (depend [v#] (.-value dom/node)))  [::synced v#]  [::pending])
             (do (set! (.-value dom/node) v#) [::synced v#]))

           (= ::err (first state#)) [::desynced (second state#)]

           (= ::ok (first state#)) [::synced (second state#)]

           :else [::pending])))

#_(defmacro input [v V! & body]
  `(dom/input
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

#_(defmacro input [v V! & body]
  `(dom/input
     (let [cv# ~v, V# ~V!, ret# (do ~@body), [status# v#] (type V#)]
       (when-not (or (= ::pending status#) (dom/focused?))  (set! (.-value dom/node) cv#))
       (dom/style {:background-color (case status#
                                       ::pending "yellow"
                                       ::failed  "red"
                                       #_else    nil)})
       (case (dbg/dbg status#) ::failed (prn ::input :err v#) nil)
       ret#)))

(defmacro input [v V! & body]
  `(dom/div (dom/style {:display "flex", :align-items "center"})
     (let [!state# (atom [::pending]), [status# v#] (e/watch !state#)]
       (svg/svg (dom/props {:viewBox "0 0 10 10", :style {:width "10px", :height "10px", :order 1, :margin-left "5px"}})
         (svg/circle (dom/props {:cx 5 :cy 5 :r 3
                                 :fill (case status# ::synced "green" ::pending "yellow" ::desynced "red")})))
       (dom/input
         (let [ret# (do ~@body)]
           (reset! !state# (type ~v ~V!))
           (case status# ::desynced (prn ::input :err v#) nil)
           ret#)))))

(defmacro textarea [v V! & body]
  `(dom/textarea
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro edn [v V! & body]
  `(dom/textarea
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-edn))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) (contrib.str/pprint-str ~v))))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(def uuid-pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
(defmacro uuid [v V! & body]
  `(dom/input (dom/props {:type "text" :pattern uuid-pattern})
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-uuid))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro long [v V! & body]
  `(dom/input (dom/props {:type "number"})
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-long))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro range [v V! & body]
  `(dom/input (dom/props {:type "range"})
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-long))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro double [v V! & body]
  `(dom/input (dom/props {:type "number"})
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-double))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro keyword [v V! & body]
  `(dom/input
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-keyword))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro symbol [v V! & body]
  `(dom/input
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-symbol))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro date [v V! & body]
  `(dom/input (dom/props {:type "date"})
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-date))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))

(defmacro datetime-local [v V! & body]
  `(dom/input (dom/props {:type "datetime-local"})
     ~(if V!
        `(each (dom/listen dom/node "input" (always (-> dom/node .-value parse-datetime-local))) ~V!
           ~(when v `(when-not (or e/busy (dom/focused?)) (set! (.-value dom/node) ~v)))
           ~@body)
        `(do (set! (.-value dom/node) ~v) ~@body))))
