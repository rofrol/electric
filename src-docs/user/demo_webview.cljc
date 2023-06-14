(ns user.demo-webview
  "Electric fullstack query/view composition with client/server transfer"
  (:require #?(:clj [contrib.datomic :as cd])
            #?(:clj [datomic.api :as d])
            [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-ui4 :as ui]
            [missionary.core :as m]))

; A database backed webview with reactive updates.
; The webview is subscribed to the database, which updates with each transaction.
; Run a transaction (from the REPL) and see the connected tabs update live.

#?(:clj
   (def schema
     [{:db/ident :order/email,  :db/valueType :db.type/string,   :db/cardinality :db.cardinality/one}
      {:db/ident :order/gender, :db/valueType :db.type/keyword,  :db/cardinality :db.cardinality/one}]))

(def db-uri "datomic:mem://webview")
;; to clear DB data replace temporarily both with `def`
#?(:clj (defonce !conn (do (d/delete-database db-uri)
                           (d/create-database db-uri)
                           (doto (d/connect db-uri)
                             (d/transact schema)
                             (d/transact [{:order/email "alice@example.com" :order/gender :order/female}
                                          {:order/email "bob@example.com" :order/gender :order/male}
                                          {:order/email "charlie@example.com" :order/gender :order/male}])))))

#?(:clj (defonce <db (->> (m/ap
                            (m/amb
                              (d/db !conn)
                              (:db-after (m/?> (cd/tx-report-queue> !conn)))))
                       (m/relieve {}) m/signal)))

(defn teeshirt-orders [db ?email]
  #?(:clj
     (sort
       (d/q '[:find [?e ...]
              :in $ ?needle :where
              [?e :order/email ?email]
              [(clojure.string/includes? ?email ?needle)]]
            db (or ?email "")))))

(e/defn Teeshirt-orders-view [db]
  (e/client
    (dom/div
      (dom/h2 (dom/text "frontend/backend webview with server push"))
      (let [!search (atom ""), search (e/watch !search)]
        (ui/input search (e/fn [v] (reset! !search v))
          (dom/props {:placeholder "Filter..."}))
        (dom/table (dom/props {:class "hyperfiddle"})
          (e/server
            (e/for [id (e/offload #(teeshirt-orders db search))]
              ;; we'd use `d/entity` is not for this Datomic bug
              ;; https://ask.datomic.com/index.php/859/equality-on-d-entity-ignores-db?show=859#q859
              (let [{:order/keys [email gender]} (d/pull db '[:order/email :order/gender] id)]
                (e/client
                  (dom/tr
                    (dom/td (dom/text id))
                    (dom/td (dom/text (e/server email)))
                    (dom/td (dom/text (e/server gender)))))))))))))

(e/defn Webview []
  (let [db (new (identity <db))] ; reactive "database value"
    (Teeshirt-orders-view. db)))

(comment
  #?(:clj (d/transact !conn [{:db/id 2 :order/email "bob2@example.com"}]))
  #?(:clj (d/transact !conn [{:order/email "dan@example.com"}]))
  #?(:clj (d/transact !conn [{:order/email "erin@example.com"}]))
  #?(:clj (d/transact !conn [{:order/email "frank@example.com"}]))
  )
