(ns hyperfiddle.electric-dom2-test
  (:require
   [contrib.cljs-target :refer [do-browser]]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]
   [hyperfiddle.rcf :as rcf :refer [% tap tests with]]
   [missionary.core :as m])
  (:import
   #?(:clj [clojure.lang ExceptionInfo])
   [hyperfiddle.electric Pending]
   [missionary Cancelled]))

(tests "dom/*for-each"
  (def !push (atom nil))
  (def !resolvers (atom {}))
  (defn resolve! [k v] (reset! (@!resolvers k) v))
  (with (e/run (tap (try (dom/*for-each (fn [pusher] (reset! !push pusher) #(do))
                           (e/fn [e]
                             (let [!v (atom :pending)]
                               (swap! !resolvers assoc e !v)
                               (try (let [v (e/watch !v)]
                                      (case v
                                        :pending  (throw (Pending.))
                                        :caught   (throw (ex-info "caught" {}))
                                        :uncaught (throw (ex-info "uncaught" {}))
                                        #_else    v))
                                    (catch Pending _ :pending)
                                    (catch #?(:clj Throwable :cljs :default) e
                                      (case (ex-message e)
                                        "caught" nil
                                        #_else   (throw e)))))))
                         (catch #?(:clj Throwable :cljs :default) e [(type e) (ex-message e)]))))
    #_init                   % := {}
    (@!push 0),              % := {0 :pending}
    (@!push 1),              % := {0 :pending, 1 :pending}
    (resolve! 1 nil),        % := {0 :pending}
    (resolve! 0 false),      % := {}
    (@!push 2),              % := {2 :pending}
    (resolve! 2 :caught),    % := {}
    (@!push 99),             % := {99 :pending}
    (resolve! 99 :uncaught), % := [ExceptionInfo "uncaught"]
    (resolve! 99 :alive),    % := {99 :alive}
    (resolve! 99 nil),       % := {}
    (tap ::done), % := ::done, (println " ok")
    )
  )

#?(:cljs
   (do-browser
     (tests "dom text node mount and unmount"
       (def !div (atom nil))
       (def !mounted? (atom true))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/div
                        (reset! !div dom/node)
                        (when (e/watch !mounted?) (dom/text "hello") :mounted))))
         (.-textContent @!div) := "hello"
         (swap! !mounted? not)
         (.-textContent @!div) := ""))

     (tests "switch on dom effect"
       (def !div (atom nil))
       (def !x (atom true))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/div
                        (reset! !div dom/node)
                        (dom/text "a")
                        (dom/text (if (e/watch !x) "b" "c"))
                        (dom/text "d"))))
         (.-textContent @!div) := "abd"
         (swap! !x not)
         (.-textContent @!div) := "acd"
         (swap! !x not)
         (.-textContent @!div) := "abd"))

     (tests "dynamic dom ordering"
       (def !div (atom nil))
       (def !xs (atom ["<" ">"]))
       (with (e/run (binding [dom/node (.-body js/document)]
                      (dom/div
                        (reset! !div dom/node)
                        (dom/text ".")
                        (e/for [x (e/watch !xs)]
                          (dom/text x))
                        (dom/text "."))))
         (.-textContent @!div) := ".<>."
         (swap! !xs reverse)
         (.-textContent @!div) := ".><."
         (swap! !xs reverse)
         (.-textContent @!div) := ".<>."))

     (tests "namespaced attributes"
       (dom/resolve-attr-alias :href) := [nil "href"]
       (dom/resolve-attr-alias "href") := [nil "href"]
       (dom/resolve-attr-alias :svg:rect) := ["http://www.w3.org/2000/svg" "rect"]
       (dom/resolve-attr-alias :xlink:href) := ["http://www.w3.org/1999/xlink" "href"]
       (dom/resolve-attr-alias "xlink:href") := ["http://www.w3.org/1999/xlink" "href"]))
   )
