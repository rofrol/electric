(ns hyperfiddle.branch-tx-test
  (:require
   #?(:clj [datomic.api :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.rcf :refer [% tap tests with]]
   [missionary.core :as m]
   [contrib.debug :as dbg]))

;; TODO
;; async transactions
;; discard 1 tx
;; find out what locks

#?(:clj
   (def schema
     [{:db/ident :task/status,      :db/valueType :db.type/keyword, :db/cardinality :db.cardinality/one}
      {:db/ident :task/description, :db/valueType :db.type/string,  :db/cardinality :db.cardinality/one}
      {:db/ident :task/id,     :db/valueType :db.type/uuid,    :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity}]))

#?(:clj (defn ->in-memory-conn [db-name schema]
          (let [uri (str "datomic:mem://" db-name)]
            (d/delete-database uri)
            (d/create-database uri)
            (let [conn (d/connect uri)]
              (d/transact conn schema)
              conn))))

#?(:clj (defn latest-db< [!conn]
          (->> (m/observe
                 (fn [!]
                   (! (d/db !conn))
                   (let [q (d/tx-report-queue !conn)
                         t (Thread. ^Runnable
                             (fn []
                               (when (try
                                       (! (:db-after (.take ^java.util.concurrent.LinkedBlockingQueue q)))
                                       true
                                       (catch InterruptedException _))
                                 (recur))))]
                     (.start t)
                     #(doto t .interrupt .join))))
            (m/relieve {}))))

(tests
  (def conn (->in-memory-conn "foo" schema))
  (def it ((latest-db< conn) #(tap :notified) #(tap :terminated)))
  % := :notified, (instance? datomic.db.Db @it) := true
  (d/transact conn [(->task "foo" (random-uuid))])
  % := :notified, (instance? datomic.db.Db @it) := true
  (d/transact conn [(->task "bar" (random-uuid))])
  % := :notified, (instance? datomic.db.Db @it) := true
  (d/transact conn [(->task "baz" (random-uuid))])
  % := :notified, (instance? datomic.db.Db @it) := true
  (it)
  )

#?(:clj (defn --stage-tx [db [tx !color]]
          (try (let [next-db (:db-after (d/with db tx))]
                 (reset! !color [::e/ok])        next-db)
               (catch Throwable e
                 (reset! !color [::e/failed e])  db))))

(e/defn Branch! [db parent]             ; parent = branch or connection
  (let [!b (atom {:parent parent, :txs []}), b (e/watch !b), txs (:txs b)]
    ;; TODO upon receiving a new tx don't rerun all of them
    (swap! !b assoc :db (reduce --stage-tx db txs))
    [!b b]))

(defn replace-or-conj [coll f v]
  (let [replaced (persistent! (reduce (fn [ac nx] (conj! ac (if (f nx) v nx))) (transient []) coll))]
    (cond-> replaced (= replaced coll) (conj v))))

(tests
  (replace-or-conj [1 2 3] (partial = 2) 0) := [1 0 3]
  (replace-or-conj []      (partial = 2) 0) := [0])

;; supposed to be called from the client
;; creates a staging fn for the client
(defmacro ->stage-fn [!b]
  `(e/fn [tx# id#]
     (try (e/server
            (let [!color# (atom [::e/pending])]
              (swap! ~!b update :txs replace-or-conj (fn [[_# _# id2#]] (= id# id2#)) [tx# !color# id#])
              (e/watch !color#)))
          (catch hyperfiddle.electric.Pending e# [::e/pending]))))

#?(:clj (defn branch? [v] (instance? clojure.lang.Atom v)))

#?(:clj (defn --commit [!b]
          ;; should we lock !b here?
          (swap! !b (fn [{:keys [parent txs] :as b}]
                      (if (branch? parent)
                        (swap! parent update :txs into txs)
                        (d/transact parent (into [] (mapcat first) txs)))
                      (assoc b :txs [])))))

;; commits this branch's staged txs into its parent
(defmacro commit! [!b] `(e/server (--commit ~!b)))

(defn- ->task [description stable-id]
  {:task/status :active, :task/description description, :task/id stable-id})

(tests "staging a tx to a Branch returns a 3-colored result"
  (def uuid (random-uuid))
  (with (e/run (try (e/server
                      (let [conn (->in-memory-conn "3color" schema)
                            global-db (new (latest-db< conn))
                            [!branch branch] (Branch!. global-db conn)
                            branch-db (:db branch)]
                        (e/client
                          (let [Stage! (->stage-fn !branch)]
                            (case (tap (first (new Stage! [(->task "foo" uuid)] (random-uuid))))
                              ::e/ok
                              (tap {::branch (e/server (:db/id (d/entity branch-db [:task/id uuid])))
                                    ::global (e/server (:db/id (d/entity global-db [:task/id uuid])))})
                              #_else nil)))))
                    (catch hyperfiddle.electric.Pending _)
                    (catch #?(:clj Throwable :cljs :default) e (prn e))))
    % := ::e/pending
    % := ::e/ok
    (def qs %)
    (number? (::branch qs)) := true
    (nil? (::global qs)) := true

    (tap ::done), % := ::done, (println " ok")))


(tests "when the parent db updates the branch db rebases on top of it"
  (def uuid1 (random-uuid))
  (def uuid2 (random-uuid))
  (def !conn (atom nil))
  (with (e/run (try (e/server
                      (let [conn (->in-memory-conn "update-parent" schema)
                            global-db (new (latest-db< conn))
                            [!branch branch] (Branch!. global-db conn)
                            branch-db (:db branch)]
                        (reset! !conn conn)
                        (e/client
                          (let [Stage! (->stage-fn !branch)]
                            (case (tap (first (new Stage! [(->task "foo" uuid1)] (random-uuid))))
                              ::e/ok
                              (tap (e/server (-> (d/datoms branch-db :avet :task/id) seq count)))
                              #_else nil)))))
                    (catch hyperfiddle.electric.Pending _)
                    (catch #?(:clj Throwable :cljs :default) e (prn e))))
    % := ::e/pending
    % := ::e/ok
    % := 1
    (d/transact @!conn [(->task "bar" uuid2)])
    % := 2

    (tap ::done), % := ::done, (println " ok")))

(tests "when the parent db updates the branch txs re-run"
  (def uuid (random-uuid))
  (def !conn (atom nil))
  (with (e/run (try (e/server
                      (let [conn (->in-memory-conn "rerun" schema)
                            global-db (new (latest-db< conn))
                            [!branch _branch] (Branch!. global-db conn)]
                        (reset! !conn conn)
                        (e/client
                          (let [Stage! (->stage-fn !branch)]
                            (tap (first (new Stage! [[:db/cas [:task/id uuid] :task/description "foo" "bar"]] (random-uuid))))))))
                    (catch hyperfiddle.electric.Pending _)
                    (catch #?(:clj Throwable :cljs :default) e (prn e))))
    % := ::e/pending
    % := ::e/failed                     ; the CAS initially fails (no entity)
    (d/transact @!conn [(->task "foo" uuid)])
    % := ::e/ok                         ; now the CAS is valid because of the transacted entity
    (d/transact @!conn [[:db/add [:task/id uuid] :task/description "quux"]])
    % := ::e/failed                     ; now the CAS is invalid because description doesn't match

    (tap ::done), % := ::done, (println " ok")))

(tests "when we commit in a branch whose parent is the connection the txs show up in the global view"
  (def uuid (random-uuid))
  (with (e/run (try (e/server
                      (let [conn (->in-memory-conn "commit" schema)
                            global-db (new (latest-db< conn))
                            [!branch _branch] (Branch!. global-db conn)]
                        (e/client
                          (let [Stage! (->stage-fn !branch)]
                            (case (tap (first (new Stage! [(->task "foo" uuid)] (random-uuid))))
                              ::e/ok
                              (case (commit! !branch)
                                (tap (e/server (:db/id (d/entity global-db [:task/id uuid])))))
                              #_else nil)))))
                    (catch hyperfiddle.electric.Pending _)
                    (catch #?(:clj Throwable :cljs :default) e (prn e))))
    % := ::e/pending
    % := ::e/ok
    (number? %) := true

    (tap ::done), % := ::done, (println " ok")))

(tests "when we commit in a branch whose parent is another branch the txs show up in the parent branch"
  (def uuid (random-uuid))
  (with (e/run (try (e/server
                      (let [conn (->in-memory-conn "commit" schema)
                            global-db (new (latest-db< conn))
                            [!branch branch] (Branch!. global-db conn)
                            [!subbranch _subbranch] (Branch!. (:db branch) !branch)]
                        (e/client
                          (let [Stage! (->stage-fn !subbranch)]
                            (case (tap (first (new Stage! [(->task "foo" uuid)] (random-uuid))))
                              ::e/ok
                              (case (commit! !subbranch)
                                (tap (e/server (into [] (mapcat first) (:txs branch)))))
                              #_else nil)))))
                    (catch hyperfiddle.electric.Pending _)
                    (catch #?(:clj Throwable :cljs :default) e (prn e))))
    % := ::e/pending
    % := ::e/ok
    % := [(->task "foo" uuid)]

    (tap ::done), % := ::done, (println " ok"))
  )

(tests "when a single tx runs twice only latest value is kept in branch txs"
  (def uuid (random-uuid))
  (def !b (atom nil))
  (def !task (atom (->task "foo" (random-uuid))))
  (with (e/run (try (e/server
                      (let [conn (->in-memory-conn "commit" schema)
                            global-db (new (latest-db< conn))
                            [!branch _branch] (Branch!. global-db conn)]
                        (reset! !b !branch)
                        (e/client
                          (let [Stage! (->stage-fn !branch)]
                            (tap (first (new Stage! [(e/watch !task)] uuid)))))))
                    (catch hyperfiddle.electric.Pending _)
                    (catch #?(:clj Throwable :cljs :default) e (prn e))))
    % := ::e/pending
    % := ::e/ok
    (count (:txs @@!b)) := 1

    (reset! !task (->task "bar" (random-uuid)))
    % := ::e/pending
    % := ::e/ok
    (count (:txs @@!b)) := 1

    (tap ::done), % := ::done, (println " ok")))
