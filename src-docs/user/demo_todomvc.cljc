(ns user.demo-todomvc
  "Requires -Xss2m to compile. The Electric compiler exceeds the default 1m JVM ThreadStackSize
  due to large macroexpansion resulting in false StackOverflowError during analysis."
  (:import [hyperfiddle.electric Pending])
  (:require
   contrib.str
   #?(:clj [datascript.core :as d])
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]))

(defonce !conn #?(:clj (d/create-conn {}) :cljs nil))       ; server
(e/def db)                                                  ; server
(e/def transact!) ; server
(def !state #?(:cljs (atom {::filter :all                   ; client
                            ::editing nil
                            ::delay   0})))

#?(:clj
   (defn query-todos [db filter]
     {:pre [filter]}
     (case filter
       :active (d/q '[:find [?e ...] :where [?e :task/status :active]] db)
       :done   (d/q '[:find [?e ...] :where [?e :task/status :done]] db)
       :all    (d/q '[:find [?e ...] :where [?e :task/status]] db))))

#?(:clj
   (defn todo-count [db filter]
     {:pre  [filter]
      :post [(number? %)]}
     (-> (case filter
           :active (d/q '[:find (count ?e) . :where [?e :task/status :active]] db)
           :done   (d/q '[:find (count ?e) . :where [?e :task/status :done]] db)
           :all    (d/q '[:find (count ?e) . :where [?e :task/status]] db))
       (or 0)))) ; datascript can return nil wtf

(e/defn Filter-control [state target label]
  (dom/a (dom/props {:class (when (= state target) "selected")})
    (dom/text label)
    (dom/on! "click" (fn [_] (swap! !state assoc ::filter target)))))


(e/defn TodoStats [state]
  (let [active (e/server (todo-count db :active))
        done   (e/server (todo-count db :done))]
    (dom/div
      (dom/span (dom/props {:class "todo-count"})
        (dom/strong (dom/text active))
        (dom/span (dom/text " " (str (case active 1 "item" "items")) " left")))

      (dom/ul (dom/props {:class "filters"})
        (dom/li (Filter-control. (::filter state) :all "All"))
        (dom/li (Filter-control. (::filter state) :active "Active"))
        (dom/li (Filter-control. (::filter state) :done "Completed")))

      (when (pos? done)
        (dom/button (dom/text "Clear completed " done)
          (let [busy (-> (dom/for-each "click"
                           (e/fn [_]
                             (try (e/server (when-some [ids (seq (query-todos db :done))]
                                              (transact! (mapv (fn [id] [:db/retractEntity id]) ids)) nil))
                                  (catch Pending _ :keep)
                                  (catch missionary.Cancelled _)
                                  (catch :default e (.error js/console e)))))
                       seq boolean)]
            (dom/props {:class "clear-completed", :disabled busy, :aria-busy busy})))))))

(e/defn TodoItem [state id]
  (e/server
    (let [{:keys [:task/status :task/description]} (d/entity db id)]
      (e/client
        (dom/li
          (dom/props {:class [(when (= :done status) "completed")
                              (when (= id (::editing state)) "editing")]})
          (dom/div (dom/props {:class "view"})
            (dom/input
              (let [busy (-> (dom/for-each "change"
                               (e/fn [e]
                                 (let [status (case (-> e .-target .-checked) true :done, false :active, nil)]
                                   (try (e/server (transact! [{:db/id id, :task/status status}]) nil)
                                        (catch Pending _ :keep)
                                        (catch missionary.Cancelled _)
                                        (catch :default e (.error js/console e))))))
                           seq boolean)]
                (dom/props {:type "checkbox", :class "toggle", :disabled busy, :aria-busy busy})
                (when-not busy (dom/bind-value (= :done status) #(set! (.-checked %) %2)))))
            (dom/label (dom/text description)
              (dom/on! "dblclick" (fn [_] (swap! !state assoc ::editing id)))))
          (when (= id (::editing state))
            (dom/span (dom/props {:class "input-load-mask"})
              (dom/props
                {:aria-busy
                 (dom/input
                   (let [busy (-> (dom/for-each "keydown"
                                    (e/fn [e]
                                      (case (.-key e)
                                        "Enter" (when-some [desc (contrib.str/blank->nil (-> e .-target .-value))]
                                                  (try (case (e/server (transact! [{:db/id id, :task/description desc}]) nil)
                                                         (swap! !state assoc ::editing nil))
                                                       nil
                                                       (catch Pending _ :keep)
                                                       (catch missionary.Cancelled _)
                                                       (catch :default e (.error js/console e))))
                                        "Escape" (swap! !state assoc ::editing nil)
                                        nil)))
                                seq boolean)]
                     (dom/props {:class "edit"})
                     (when-not busy (dom/bind-value description))
                     (case description ; HACK sequence - run focus after description is available
                       (.focus dom/node))
                     busy))})))
          (dom/button
            (let [busy (-> (dom/for-each "click"
                             (e/fn [_] (try (e/server (transact! [[:db/retractEntity id]]) nil)
                                            (catch Pending _ :keep)
                                            (catch missionary.Cancelled _)
                                            (catch :default e (.error js/console e)))))
                         seq boolean)]
             (dom/props {:class "destroy", :disabled busy, :aria-busy busy}))))))))

#?(:clj
   (defn toggle-all! [db status]
     (let [ids    (query-todos db (if (= :done status) :active :done))]
       (map (fn [id] {:db/id id, :task/status status}) ids))))

(e/defn TodoList [state]
  (e/client
    (dom/div
      (dom/section (dom/props {:class "main"})
        (let [active (e/server (todo-count db :active))
              all    (e/server (todo-count db :all))
              done   (e/server (todo-count db :done))]
          (dom/input
            (let [busy (-> (dom/for-each "change"
                             (e/fn [e]
                               (let [status (case (-> e .-target .-checked) (true nil) :done, false :active)]
                                 (try (e/server (transact! (toggle-all! db status)) nil)
                                      (catch Pending _ :keep)
                                      (catch missionary.Cancelled _)
                                      (catch :default e (.error js/console e))))))
                         seq boolean)]
              (when-not busy
                (let [checked (cond (= all done) true, (= all active) false, :else nil)]
                  (dom/bind-value checked #(set! (.-checked %) %2))))
              (dom/props {:type "checkbox", :class "toggle-all", :disabled busy, :aria-busy busy}))))
        (dom/label (dom/props {:for "toggle-all"}) (dom/text "Mark all as complete"))
        (dom/ul (dom/props {:class "todo-list"})
          (e/for [id (e/server (sort (query-todos db (::filter state))))]
            (TodoItem. state id)))))))

#?(:cljs (defn submit? [e] (and (= "Enter" (.-key e)) (not= "" (-> e .-target .-value)))))
#?(:cljs (defn read-value! [node] (let [v (.-value node)] (set! (.-value node) "") v)))

;; FIXME unreliable focus behavior after Enter, did this regress?
(e/defn CreateTodo []
  (dom/span (dom/props {:class "input-load-mask"})
    (dom/props
      {:aria-busy
       (dom/input
         (let [busy (-> (dom/for-each "keydown"
                          (e/fn [e]
                            (when (submit? e)
                              (let [description (read-value! dom/node)]
                                (try (e/server (transact! [{:task/description description, :task/status :active}]) nil)
                                     (catch Pending _ :keep)
                                     (catch missionary.Cancelled _)
                                     (catch :default e (.error js/console e)))))))
                      seq boolean)]
           (dom/props {:class "new-todo", :placeholder "What needs to be done?", :disabled busy, :aria-busy busy})
           busy))})))

(e/defn TodoMVC-UI [state]
  (dom/section (dom/props {:class "todoapp"})
    (dom/header (dom/props {:class "header"})
      (CreateTodo.))
    (when (e/server (pos? (todo-count db :all)))
      (TodoList. state))
    (dom/footer (dom/props {:class "footer"})
      (TodoStats. state))))

(e/defn TodoMVC-body [state]
  (dom/div (dom/props {:class "todomvc"})
    (dom/h1 (dom/text "TodoMVC"))
    (TodoMVC-UI. state)
    (dom/footer (dom/props {:class "info"})
      (dom/p (dom/text "Double-click to edit a todo")))))

(e/defn Diagnostics [state]
  (dom/h1 (dom/text "Diagnostics"))
  (dom/dl
    (dom/dt (dom/text "count :all")) (dom/dd (dom/text (pr-str (e/server (todo-count db :all)))))
    (dom/dt (dom/text "query :all")) (dom/dd (dom/text (pr-str (e/server (query-todos db :all)))))
    (dom/dt (dom/text "state")) (dom/dd (dom/text (pr-str state)))
    (dom/dt (dom/text "delay")) (dom/dd
                                  (dom/input (dom/props {:type "number" :step 1, :min 0, :style {:width :min-content}})
                                    (when-some [v (parse-long (dom/Value.))]
                                      (swap! !state assoc ::delay v )))
                                  (dom/text " ms"))))

#?(:clj
   (defn slow-transact! [!conn delay tx]
     (try (Thread/sleep delay) ; artificial latency
          (d/transact! !conn tx)
          (catch InterruptedException _))))

(e/defn TodoMVC []
  (e/client
    (let [state (e/watch !state)]
      (e/server
        (binding [db (e/watch !conn)
                  transact! (partial slow-transact! !conn (e/client (::delay state)))]
          (e/client
            (dom/link (dom/props {:rel :stylesheet, :href "/todomvc.css"}))
            ; exclude #root style from todomvc-composed by inlining here
            (dom/element "style" (dom/text "body.hyperfiddle { width: 65vw; margin-left: auto; margin-right: auto; }"))
            (TodoMVC-body. state)
            #_(Diagnostics. state)))))))

(comment
  (todo-count @!conn :all)
  (todo-count @!conn :active)
  (todo-count @!conn :done)
  (query-todos @!conn :all)
  (query-todos @!conn :active)
  (query-todos @!conn :done)
  (d/q '[:find (count ?e) . :where [?e :task/status]] @!conn)
  )
