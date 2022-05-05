(ns user.orders-ui
  (:require [hyperfiddle.photon :as p]
            [missionary.core :as m]))
(def !coll (atom [9]))

(p/defn Orders []
  ;; client
  ~@;; server
    (new
     (first ; join the first thunk produced by p/for
      (p/for [_ (new (m/watch !coll))] ; return a list of thunks
        (p/fn []
          (new (p/fn [] "hello")))))))

(comment
  (reset! !coll [10]))

;; Client will receive
;;  "hello" on first run
;;  "hello" when !coll is resetted

;; Server event-loop thread deadlock immediately on !coll reset!.

;; Reproducible only in a real client/server setup.

;; ⚠️ You can get your REPL back by interrupting evaluation. It will not unlock
;; the event loop thread. If you refresh the page and the server picks a locked
;; event loop thread to run the reactor, the page will appear stuck. Refresh
;; until the server picks an health event loop thread. Or restart your JVM.