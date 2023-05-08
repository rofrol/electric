(ns wip.crud-controls
  #?(:cljs (:require-macros wip.crud-controls))
  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.electric-crud :as crud]
            [missionary.core :as m]
            [contrib.debug :as dbg]))

 (defmacro after
  ([ms form] `(case (new (e/task->cp (m/sleep ~ms))) ~form))
  ([ms pending form] `(case (new (e/task->cp (m/sleep ~ms ::done) ~pending)) ::done ~form nil)))

(e/defn CrudControls []
  (e/client
    (crud/button (e/fn [] (after 200 (when (zero? (rand-int 2)) (throw (ex-info "ZERO!" {})))))
      (dom/text "4 colored button"))))
