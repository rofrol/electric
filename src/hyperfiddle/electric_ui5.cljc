(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui4))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui4]
            [missionary.core :as m]))

(defmacro control [event-type parse unparse v V! setter & body]
  `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
                       (some->> (~parse e#) (new ~V!)))]
     (dom/style {:background-color (when (= ::e/pending state#) "yellow")})
     ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
     (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
       (~setter dom/node (~unparse v#))) ; js coerce
     ~@body
     [state# v#]))

(defmacro entity [record EnsureEntity & body]
  `(dom/div (dom/text "entity" (:hf/stable-id ~record))
     (let [[state# e#] (new ~EnsureEntity (:db/id ~record) ~record)
           color# (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red")]  ; tooltip
       (dom/style {:border (str "2px solid " color#)})
       ~@body)))

(defmacro checkbox [record V V! EnsureEntity & body]
  `(dom/input (dom/props {:type "checkbox"})
     (let [[state# v#] (control "change" checked identity ~record ~V! #(set! (.-checked %) %2)
                         ~@body)]

       (let [[state# e#] (new ~EnsureEntity (:db/id ~record) ~record)
             v# (new ~V e#)
             color# (case state# 
                      ::e/init "gray"
                      ::e/ok "green"
                      ::e/pending "yellow"
                      ::e/failed "red")] ; tooltip
         (dom/style {:outline (str "2px solid " color#)})

         #_(case state#
             ::e/pending (dom/text "⌛ " v#)
             ::e/failed
             (do (dom/text "💀 " v#)
               (ui/button (e/fn [] #_(reset! !err nil)) ; retry
                 (dom/text "⟳")) (dom/text " (" (ex-message v#) ")"))
             (::e/ok ::e/init) .)))))