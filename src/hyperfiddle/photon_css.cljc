(ns hyperfiddle.photon-css
  (:refer-clojure :exclude [class])
  (:require [clojure.string :as str]
            [hyperfiddle.rcf :as rcf :refer [tests]]
            [hyperfiddle.photon-dom2 :as-alias dom]
            #?(:clj [clojure.java.io :as io])

            [clojure.edn :as edn])
  #?(:cljs (:require-macros [hyperfiddle.photon-css])))

;; For some CSSOM context (what react does is not interersting)
;; "https://medium.com/dailyjs/high-performance-dynamic-styles-db28c873940"

(defn rules-text [str]
  (-> str
    (str/replace #";+" ";")
    (str/replace #"\r?\n+" "")
    (str/replace #"}\s+" "}")
    (str/replace #"\s+}" "}")
    (str/replace #"\s+;" ";")
    (str/replace #";\s+" ";")
    (str/replace #":\s+" ":")
    (str/replace #"\s+:" ":")
    (str/replace #"}" "}\n")
    (str/split #"\n")
    )
  )

(tests
  (def CSS
    "* > label {color:red;;  } 
   pre:hover{color:$color;} p {color : green ;}
")
  (rules-text CSS) := ["* > label {color:red;}" "pre:hover{color:$color;}" "p {color:green;}"])

;; JVM only, match $(sexpr) using forward references: http://stackoverflow.com/a/47162099
;; (?=\()(?:(?=.*?\((?!.*?\1)(.*\)(?!.*\2).*))(?=.*?\)(?!.*?\2)(.*)).)+?.*?(?=\1)[^(]*(?=\2$)
(def SEXPR-HOLE #?(:clj #"\$(?=\()(?:(?=.*?\((?!.*?\1)(.*\)(?!.*\2).*))(?=.*?\)(?!.*?\2)(.*)).)+?.*?(?=\1)[^(]*(?=\2$)"))
(def VALUE-HOLE #"(\$[^\(\s]*)")

(defn holes [str]
  (->> (concat (map first (re-seq SEXPR-HOLE str))
         (map first (re-seq VALUE-HOLE str)))
    (remove #{"$"})
    (sort-by #(.indexOf str %))))

(tests
  (holes "color:$red $black") := ["$red" "$black"]
  (holes "color: $(identity \"red\") $(str (inc foo)) ") := ["$(identity \"red\")" "$(str (inc foo))"]
  )

;; TODO template should accept sexprs (need to regexp match balanced parens)
;; Desired syntax : $value or $(sexpr arg0 arg1)
(defn template [rhs]
  (let [hole-form (fn [hole] (clojure.edn/read-string (str/replace-first hole #"\$" "")))
        holes     (holes rhs)]
    (if (empty? holes)
      rhs
      (let [left  (as-> holes $
                    (reduce (fn [rhs hole] (str/replace-first rhs hole "_HF_CSS_CUT_")) rhs $)
                    (str/split $ #"_HF_CSS_CUT_"))
            right (map hole-form holes)]
        (cons `str
          (if (empty? left)
            right
            (butlast (interleave left (concat right (repeat (count left) nil))))))))))

(tests
  (template "1fr $var 2fr $(var2) 3fr;")
  := `(str "1fr " ~'var " 2fr " (~'var2) " 3fr;"))

(defn parse-declaration [decl] (map str/trim (str/split decl #":")))

(defn parse-rule [rule-text]
  (let [[selector & declarations] (-> rule-text
                                    (str/replace #"};?$" "")
                                    (str/split #"(\s*\{|;)"))]
    [(str/trim selector) (mapv parse-declaration declarations)]))

(defn template-rule [rules-text]
  (let [[selector declarations] (parse-rule rules-text)]
    [selector (mapv (fn [[key value]] [key (template value)]) declarations)]))

(tests
  (map template-rule (rules-text CSS))
   := '(["* > label" (["color" "red"])]
        ["pre:hover" (["color" (clojure.core/str color)])]
        ["p" (["color" "green"])]))

(defn make-rule [node selector]
  (let [idx (.insertRule (.-sheet node) (str selector " {}"))]
    (aget (.-cssRules (.-sheet node)) idx)))

(defn make-selector [static dynamic selector]
  (str/triml
    (str
      (when static (str "." static))
      (when dynamic (str "." dynamic))
      " " selector)))

(defn emit-declarations [decls]
  (str "{\n" (str/join ";\n" (map #(str "  "(str/join ": " %)) decls)) "\n}"))

(defn dynamic? [[_key value :as _declaration]] (seq? value))

;; TODO integrate static css generation into shadow build
;; - Target file should be set by user
;; - Emitted rules should dedupe and always produce a clean file
(defn write-static-css! [static-class selector declarations]
  #?(:clj
     (let [static-decls (remove dynamic? declarations)]
       (when (seq static-decls)
         (let [selector (make-selector static-class nil selector)
               out      (str selector (emit-declarations static-decls) \newline)
               file     (io/file "resources/public/generated.css")
               ]
           (.createNewFile file)        ; noop if already exists
           (spit file out :append true))))))

(defn css-rule*
  ([rule-text] (css-rule* nil nil rule-text))
  ([static-class dynamic-class rule-text]
   (let [[selector declarations] (template-rule rule-text)
         ;; dynamic-decls           (filter dynamic? declarations)
         rule-sym                (gensym "rule_")]
     ;; (write-static-css! static-class selector declarations)
     (when (seq declarations)
       `(let [~rule-sym (.-style (make-rule dom/node (make-selector ~static-class ~dynamic-class ~selector)))]
          ~@(map (fn [[key value]]
                   `(.setProperty ~rule-sym ~key ~value))
              declarations))))))

(defmacro css-rule
  ([rule-text] (css-rule* rule-text))
  ([static-class dynamic-class rule-text] (css-rule* static-class dynamic-class rule-text)))

(defn dynamic-class [] (str (munge (gensym))))

(defn css* [static-class CSS]
  (let [dynamic-sym (gensym "dynamic")]
    `(let [~dynamic-sym (dynamic-class)]
       (dom/element "style"
         ~@(map (fn [rule] `(css-rule ~static-class ~dynamic-sym ~rule)) (rules-text CSS)))
       [~static-class ~dynamic-sym])))

(defmacro css
  ([CSS] (css* nil CSS))
  ([static-class CSS] (css* static-class CSS)))

(defn place-name [env form]
  (let [{:keys [line column]} (meta form)
        ns                    (some-> (:name (:ns env)) str (str/replace #"\." "-")) ]
    (cond-> nil
      ns     (str ns)
      line   (str "_" line)
      column (str "_" column))))

(defmacro styled [tag cssText & body]
  `(let [[static# dynamic#] (css ~(place-name &env &form) ~cssText)]
     (~tag
      (dom/props {:class [static# dynamic#]})
      ~@body
      )))

(defn class
  ([] (str (gensym "class_")))
  ([prefix] (str (munge (gensym prefix)))))
