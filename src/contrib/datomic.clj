(ns contrib.datomic
  (:require [datomic.api :as d]
            [missionary.core :as m]))

(defn tx-report-queue> [conn]
  (m/ap
    (let [q (m/?> (m/observe (fn [!] (! (d/tx-report-queue conn)) #(d/remove-tx-report-queue conn))))]
      (m/? (m/?> (m/seed (repeat (m/via m/blk (.take ^java.util.concurrent.BlockingQueue q)))))))))
