(ns user.demo-system-properties
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric :as e]
   [hyperfiddle.electric-dom2 :as dom]))

; A web view that queries the backend JVM environment and writes it to the
; frontend dom, all in a single composed expression.
; The p/for is stabilized with a "react key" for efficient DOM maintenance.

#?(:clj
   (defn jvm-system-properties [?s]
     (->> (System/getProperties)
       (filter (fn [[k v]] (str/includes? (str/lower-case (str k)) (str/lower-case (str ?s)))))
       (into {}))))

(e/defn SystemProperties []
  (e/client
    (dom/h1 (dom/text "JVM System Properties search"))
    (dom/div (dom/props {:style {:display "flex", :flex-direction "column"}})
      (let [search (dom/input (dom/props {:type "search", :placeholder "java.home"}) (dom/Value.))]
        (e/server
          (let [system-props (e/offload #(sort-by key (jvm-system-properties search)))
                matched-count (count system-props)]
            (e/client
              (dom/table
                (e/server
                  (e/for-by first [[k v] system-props]
                    (e/client
                      (dom/tr
                        (dom/td (dom/text k))
                        (dom/td (dom/props {:style {:white-space :nowrap}}) (dom/text v)))))))
              (dom/div (dom/props {:style {:color "gray", :order -1}}) (dom/text matched-count " matches")))))))))
