(ns hyperfiddle.electric-client
  (:require [contrib.cljs-target :refer [do-browser]]
            [missionary.core :as m]
            [hyperfiddle.electric.impl.runtime :as r]
            [hyperfiddle.electric.impl.io :as io])
  (:import missionary.Cancelled))

(goog-define VERSION "")

(do-browser
  (defn server-url []
    (let [url (new js/URL (.-location js/window))
          proto (.-protocol url)]
      (set! (.-protocol url)
        (case proto
          "http:" "ws:"
          "https:" "wss:"
          (throw (ex-info "Unexpected protocol" proto))))
      (.. url -searchParams (set "HYPERFIDDLE_ELECTRIC_CLIENT_VERSION" VERSION))
      (set! (.-hash url) "") ; fragment is forbidden in WS URL https://websockets.spec.whatwg.org/#ref-for-dom-websocket-websocket%E2%91%A0
      (.toString url))))

(def ^:dynamic *ws-server-url* (do-browser (server-url)))

(defn remove-listeners [ws]
  (set! (.-onopen ws) nil)
  (set! (.-onclose ws) nil))

(defn connect [url]
  (fn [s f]
    (try
      (let [ws (new js/WebSocket url)]
        (set! (.-binaryType ws) "arraybuffer")
        (set! (.-onopen ws)
          (fn [_]
            (remove-listeners ws)
            (s ws)))
        (set! (.-onclose ws)
          (fn [_]
            (remove-listeners ws)
            (s nil)))
        #(when (= (.-CONNECTING js/WebSocket) (.-readyState ws))
           (.close ws)))
      (catch :default e
        (f e) #()))))

(defn wait-for-flush [ws]
  (m/sp
    (while (< 4096 (.-bufferedAmount ws))
      (m/? (m/sleep 50)))))

(defn payload [x]
  (.-data x))

(defn send! [ws msg]
  (doto ws (.send msg)))

(defn send-all [ws msgs]
  (m/reduce {} nil (m/ap (m/? (wait-for-flush (send! ws (io/encode (m/?> msgs))))))))

(defn keepalive! [ws delay message]
  ;; temporary hack. We should emit pings X seconds after the LAST message, not every X seconds. Need a fix for immediate switch in `ap`.
  (m/sp (do-browser
          (let [!visible (atom [(.-visibilityState js/document) (.now js/Date)])
                cancel ((m/reduce #(reset! !visible %2) nil
                          (m/observe (fn [!]
                                       (let [cb (fn [_] (! [(.-visibilityState js/document) (.now js/Date)]))]
                                         (.addEventListener js/document "visibilitychange" cb)
                                         #(.removeEventListener js/document "visibilitychange" cb)))))
                        identity identity)]
            (try (loop []
                   (m/? (m/sleep delay))
                   (let [now (.now js/Date), [state since] @!visible]
                     (when (or (= "visible" state) (> delay (- now since)))
                       (.send ws message)
                       (recur))))
                 (finally (cancel)))))))

(def ELECTRIC-CLIENT-HEARTBEAT-INTERVAL 45000)

(defn connector "
server : the server part of the program
cb : the callback for incoming messages.
msgs : the discrete flow of messages to send, spawned when websocket is connected, cancelled on websocket close.
Returns a task producing nil or failing if the websocket was closed before end of reduction.
" [server]
  (fn [cb msgs]
    (m/sp
      (if-some [ws (m/? (connect *ws-server-url*))]
        (let [dfv (m/dfv)
              closer #(dfv %)]          ; doesn't work in onclose otherwise, why?
          (try
            (set! (.-onclose ws) closer)
            (send! ws (io/encode server))
            (set! (.-onmessage ws) (comp cb io/decode payload))
            (try (m/? (m/race (send-all ws msgs) dfv
                        (keepalive! ws ELECTRIC-CLIENT-HEARTBEAT-INTERVAL "HEARTBEAT")))
                 (catch ExceptionInfo e
                   (when-not (= "Race failure." (ex-message e))
                     (throw e))))
            (finally
              (when-not (= (.-CLOSED js/WebSocket) (.-readyState ws))
                (.close ws))))
          (let [close-event (m/? (m/compel dfv))]
            {:code (.-code close-event), :reason (.-reason close-event)}))
        {}))))

(defn fib-iter [[a b]]
  (case b
    0 [1 1]
    [b (+ a b)]))

(def fib (map first (iterate fib-iter [1 1])))

(comment (take 5 fib2) := [1 1 2 3 5])

(def retry-delays (map (partial * 100) fib))

(comment (take 5 retry-delays))

(def wait-for-tab-visibility
  (do-browser
    (let [end-task (->> (m/observe (fn [!]
                                     (let [cb (fn [_] (! (.-visibilityState js/document)))]
                                       (cb nil)
                                       (.addEventListener js/document "visibilitychange" cb)
                                       #(.removeEventListener js/document "visibilitychange" cb))))
                     (m/eduction (map #(when (= "visible" %) (reduced (.now js/Date)))))
                     (m/reduce {} nil))]
      (m/sp (let [init (.now js/Date)] (- (m/? end-task) init))))))

(defn boot-with-retry [client conn]
  (m/sp
    (try
      (let [ws-server-url *ws-server-url*]
        (loop [delays retry-delays]
          (let [s (object-array 1)]
            (.log js/console "Connecting...")
            (when-some [[delay & delays]
                        (when-some [info (binding [*ws-server-url* ws-server-url]
                                           (m/? (conn (fn [x] ((aget s 0) x))
                                                  (m/ap
                                                    (.log js/console "Connected.")
                                                    (let [r (m/rdv)]
                                                      (m/amb=
                                                        (do (m/? (client r (r/subject-at s 0)))
                                                            (m/amb))
                                                        (loop []
                                                          (if-some [x (m/? r)]
                                                            (m/amb x (recur))
                                                            (m/amb)))))))))]
                          (if-some [code (:code info)]
                            (case code ; https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1
                              (1005 1006) (do (.log js/console "Connection lost.") (seq retry-delays))
                              (1008) (throw (ex-info "Stale client" {:hyperfiddle.electric/type ::stale-client}))
                              (1013) (do (.log js/console "Server timed out, considering this client inactive.")
                                         (seq retry-delays))
                              (throw (ex-info (str "Remote error - " code " " (:reason info)) {})))
                            (do (.log js/console "Failed to connect.") delays)))]
              (if (= "visible" (.-visibilityState js/document))
                (do (.log js/console (str "Next reconnect attempt in " (/ delay 1000) " seconds."))
                    (recur (m/? (m/sleep delay delays))))
                (do (.log js/console "waiting for tab visibility...")
                    (m/? wait-for-tab-visibility)
                    (.log js/console "Attempting to reconnect")
                    (recur delays)))))))
      (catch Cancelled _))))

(defn reload-when-stale [task]
  (fn [s f]
    (task s (fn [error]
              (when (= ::stale-client (:hyperfiddle.electric/type (ex-data error)))
                (do (js/console.log "Server and client version mismatch. Refreshing page.")
                  (.reload (.-location js/window))))
              (f error)))))
