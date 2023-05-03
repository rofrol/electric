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

(defmacro do1 [x & body] `(let [ret# ~x] ~@body ret#))

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

(defmacro pending? [v]
  `(try ~v false
        (catch hyperfiddle.electric.Pending _ true)
        (catch ~(if (:env &env) :default 'Throwable) _ false)))

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
     (if (seq pending#) [::pending] state#)))

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

;; TODO is this better?
;; (depend [v#] .-value dom/node)
(defmacro depend [deps form]
  (let [args (vec (repeatedly (+ (dec (count form)) (count deps)) gensym))]
    `((fn ~args (~(first form) ~@(take (dec (count form)) args)))
      ~@(rest form) ~@deps)))

(defmacro type
  ([v V!] `(type ~v ~V! (always (-> dom/node .-value))))
  ([v V! keep-fn]
   `(let [v# ~v, v-pending?# (pending? v#)
          !state# (atom [::init]), state# (e/watch !state#)
          pending# (e/for-event [text# (dom/listen dom/node "input" ~keep-fn)]
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

            :else [::pending]))))

(e/defn Dot [status]
  (svg/svg (dom/props {:viewBox "0 0 10 10", :style {:width "10px", :height "10px", :order 1, :margin-left "5px"}})
         (svg/circle (dom/props {:cx 5 :cy 5 :r 3
                                 :fill (case status ::synced "green" ::pending "yellow" ::desynced "red")}))))

(e/defn WithDot [Writer]
  (dom/div (dom/style {:display "flex", :align-items "center"})
     (let [!state (atom [::pending]), [status v] (e/watch !state)]
       (new Dot status)
       (case status ::desynced (throw v) nil)
       (new Writer !state status v))))

(defmacro input [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input (do1 (do ~@body) (reset! !state# (type ~v ~V!)))))))

(defmacro textarea [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/textarea (do1 (do ~@body) (reset! !state# (type ~v ~V!)))))))

(defmacro edn [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/textarea
         (do1 (do ~@body)
           (reset! !state# (type (contrib.str/pprint-str ~v) ~V! (always (-> dom/node .-value parse-edn)))))))))

(def uuid-pattern "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
(defmacro uuid [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input (dom/props {:type "text", :pattern uuid-pattern})
         (do1 (do ~@body)
           (reset! !state# (type ~v ~V! (always (-> dom/node .-value parse-uuid)))))))))

(defmacro long [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input (dom/props {:type "number"})
         (do1 (do ~@body)
           (reset! !state# (type ~v ~V! (always (-> dom/node .-value parse-long)))))))))

(defmacro range [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input (dom/props {:type "range"})
         (do1 (do ~@body)
           (reset! !state (type ~v ~V! (always (-> dom/node .-value parse-long)))))))))

(defmacro double [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input (dom/props {:type "number"})
         (do1 (do ~@body)
           (reset! !state (type ~v ~V! (always (-> dom/node .-value parse-double)))))))))

(defmacro keyword [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input
         (do1 (do ~@body)
           (reset! !state (type ~v ~V! (always (-> dom/node .-value parse-keyword)))))))))

(defmacro symbol [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input
         (do1 (do ~@body)
           (reset! !state (type ~v ~V! (always (-> dom/node .-value parse-symbol)))))))))

(defmacro date [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input (dom/props {:type "date"})
         (do1 (do ~@body)
           (reset! !state (type ~v ~V! (always (-> dom/node .-value parse-date)))))))))

(defmacro datetime-local [v V! & body]
  `(new WithDot
     (e/fn [!state# status# v#]
       (dom/input (dom/props {:type "datetime-local"})
         (do1 (do ~@body)
           (reset! !state (type ~v ~V! (always (-> dom/node .-value parse-datetime-local)))))))))
