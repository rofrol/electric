(ns peter.y2023.dom-event-queue)

;; tucked away since there is no use case yet

(comment
  (defn- q [] #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))

  (e/defn EventQueue [type busy]
    (peek
      (let [!state (atom [:idle (q) nil]), [status q e :as state] (e/watch !state)]
        (on! type (partial swap! !state (fn [[status q e] newe]
                                          (case status
                                            :idle [:impulse q newe]
                                            :impulse [:impulse (conj q newe) e]
                                            :pending [:pending (conj q newe) e]))))
        (reset! !state
          (case status
            :idle (if (seq q) [:impulse (pop q) (peek q)] state)
            :impulse [:pending q e]
            :pending (if busy state [:idle q nil]))))))

  (defmacro -on [node Strategy typ F]
    `(binding [node ~node]
       (let [[state# v#] (e/with-cycle [x# [::init nil]]
                           (if-some [evt# (new ~Strategy ~typ (= (first x#) ::pending))]
                             (try [::ok (new ~F evt#)]
                                  (catch Pending  e# [::pending e#])
                                  (catch :default e# [::err e#]))
                             x#))]
         (case state# (::init ::ok) v#, (::err ::pending) (throw v#)))))

  (defmacro on-bp
    ([typ F] `(on-bp node ~typ ~F))
    ([node typ F] `(-on ~node EventSequence ~typ ~F)))

  (defmacro on-queued
    ([typ F] `(on-queued node ~typ ~F))
    ([node typ F] `(-on ~node EventQueue ~typ ~F)))
  )
