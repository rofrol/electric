(ns hyperfiddle.electric-ui5
  #?(:cljs (:require-macros hyperfiddle.electric-ui5))
  (:refer-clojure :exclude [long double keyword symbol uuid range])
  (:require clojure.edn
            contrib.str
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [missionary.core :as m]
            [contrib.debug :as dbg]))

;; (defn tempid? [x] (string? x))

;; (e/defn Read-entity [id]
;;   (try
;;     (e/server [::e/init (into {} (d/touch (d/entity db id)))])
;;     (catch Pending _ [::e/pending nil])
;;     (catch :default e [::e/failed e])))

;; (e/defn Create-entity [id record]
;;   (try ; create is never ::e/init
;;     (e/server
;;       (new Tx! [record]) ; returns tx-report which has :ids->tempids
;;       [::e/ok (into {} (d/touch (d/entity db id)))])
;;     (catch Pending _ [::e/pending record]) ; optimistic
;;     (catch :default e [::e/failed e])))

;; (e/defn Ensure-entity [id record]
;;   (if-not (tempid? id)
;;     (Read-entity. id)
;;     (Create-entity. id record))) ; todo must be idempotent

;; (defmacro control [event-type parse unparse v V! setter & body]
;;   `(let [[state# v#] (e/for-event-pending-switch [e# (e/listen> dom/node ~event-type)]
;;                        (some->> (~parse e#) (new ~V!)))]
;;      #_(dom/style {:background-color (when (= ::e/pending state#) "yellow")})
;;      ; workaround "when-true" bug: extra outer when-some added to guard a nil from incorrectly sneaking through
;;      (when-some [v# (when (and (not (new dom/Focused?)) (#{::e/init ::e/ok} state#)) ~v)]
;;        (~setter dom/node (~unparse v#))) ; js coerce
;;      ~@body
;;      [state# v#]))

(e/def local?)
(def tempid? (some-fn nil? string?))

(defmacro entity [record EnsureEntity & body]
  `(let [[state# e#] (new ~EnsureEntity (:db/id ~record) ~record)]
     (case state# ::e/failed (.error js/console e#) nil)
     ;; mark entity local for downstream code
     (binding [local? (tempid? (:db/id ~record))]
       ~@body)))

(defmacro input [v V! & body]
  `(dom/input
     (let [[state# v#] (control "input" #(-> % .-target .-value) identity ~v ~V! dom/set-val)
           color# (if local? "blue" (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red"))]
       (dom/style {:border-width "2px", :border-style "solid", :border-color color#, :border-radius "0.2em"})
       (when local? (dom/props {:disabled true}))
       (case state# ::e/failed (.error js/console v#) nil)
       ~@body)))

;; (defmacro checkbox [v V! & body]
;;   `(dom/input (dom/props {:type "checkbox"})
;;      (let [[state# v#] (control "change" #(-> % .-target .-checked) identity ~v ~V! #(set! (.-checked %) %2))
;;            color# (if local? "blue" (case state# ::e/init "gray", ::e/ok "green", ::e/pending "yellow", ::e/failed "red"))]
;;        (dom/style {:outline-width "2px", :outline-style "solid", :outline-color color#})
;;        (when local? (dom/props {:disabled true}))
;;        (case state# ::e/failed (.error js/console v#) nil)
;;        ~@body)))

(defmacro checkbox [record V V! & body]
  `(dom/input (dom/props {:type "checkbox"})
     (let [[state# v#] (control' "change" checked identity ~v ~V! #(set! (.-checked %) %2)
                         ~@body)]

       (let [[state# e#] (Ensure-entity. ~(:db/id record) ~record)
             v# (~V. e#)
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
