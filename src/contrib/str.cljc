(ns contrib.str
  (:refer-clojure :exclude [empty?])
  (:require clojure.pprint
            clojure.string
            [contrib.data :refer [orp]]
            [hyperfiddle.rcf :refer [tests]])
  #?(:cljs (:import [goog.i18n MessageFormat DateTimeFormat]
                    [goog.i18n.DateTimeFormat Format])))

(defn pprint-str [x]
  (with-out-str
    (binding [clojure.pprint/*print-right-margin* 100
              #_#_clojure.pprint/*print-miser-width* 1
              #_#_clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
      (clojure.pprint/with-pprint-dispatch
        clojure.pprint/code-dispatch
        (clojure.pprint/pprint x)))))

(comment
  (pprint-str (range 50))
  (pprint-str '{(user.gender-shirt-size/submissions "" .)
                [{:db/id 9}
                 {:db/id 10}
                 {:db/id 11}]})
  (pprint-str '{(user.gender-shirt-size/submissions "bob" .)
                [{:dustingetz/gender {:db/ident :dustingetz/male},
                  :dustingetz/email "bob@example.com",
                  :dustingetz/shirt-size {:db/ident :dustingetz/mens-large},
                  :db/id 10}]}))

(defn ^:deprecated includes-str? [v needle]
  ; perf - https://clojurians.slack.com/archives/C03RZMDSH/p1666290300539289
  ; want to reduce memory pressure moreso than optimzie the speed
  ; if you want a case-insensitive match I recommend using something which doesn’t force new string
  ; allocations; use org.apache.commons.lang3.StringUtils.containsIgnoreCase() which uses
  ; String.regionMatches under the hood. Or better yet use d/filter on d/datoms (can we pass that
  ; as an input to another query?)

  ; Is this deprecated because of performances?
  (clojure.string/includes? (clojure.string/lower-case (str v))
                            (clojure.string/lower-case (str needle))))

(tests
  (includes-str? "alice" "e") := true
  (includes-str? "alice" "f") := false
  (includes-str? "alice" "") := true
  (includes-str? "alice" nil) := true
  (includes-str? nil nil) := true
  (includes-str? nil "") := true
  (includes-str? "" nil) := true)

(defn any-matches? [coll needle]
  (let [substr (clojure.string/lower-case (str needle))]
    (some #(when % (clojure.string/includes? (clojure.string/lower-case (str %)) substr)) coll)))

(tests
  (any-matches? [1 2 nil 3] "3") := true
  (any-matches? ["xyz"] "Y") := true
  (any-matches? ["ABC"] "abc") := true
  (any-matches? ["abc"] "d") := nil)


(defn empty? [s] (or (and (string? s) (zero? (count s)))
                     (nil? s)))

(tests
  (empty? "") := true
  (empty? nil) := true
  (empty? " ") := false)

(defn empty->nil [s] (if (empty? s) nil s))

(tests
  (empty->nil nil) := nil
  (empty->nil "") := nil
  (empty->nil " ") := " "
  (empty->nil "a") := "a")

(defn blank->nil "Nullify empty strings, identity on all other values." [s]
  (if-not (string? s)
    s ; don't fail
    (if-not (clojure.string/blank? s) s nil)))

(tests
  (blank->nil nil) := nil
  (blank->nil "") := nil
  (blank->nil " ") := nil
  (blank->nil "      ") := nil
  (blank->nil "a") := "a"
  (not= (blank->nil "   a") nil) := true
  (not= (blank->nil "   a   ") nil) := true
  (blank->nil 1) := 1
  (blank->nil nil) := nil)

(defn or-str
  #_([& args] (apply orp seq args))                         ; can't apply macro todo
  ([a b] (orp seq a b))
  ([a b c] (orp seq a b c)))

(tests
  (or-str nil "b") := "b"
  (or-str "" "b") := "b"
  (or-str "a" "b") := "a"
  (or-str " " "b") := " ")

#?(:cljs
   (defn message "
Given a template, return a function taking a map of template arguments and returning a formatted message string.

Template example:
\"I see {NUM_PEOPLE, plural, offset:1
         =0 {no one at all}
         =1 {{WHO}}
         one {{WHO} and one other person}
         other {{WHO} and # other people}}
 in {PLACE}.\"

 Providing {'NUM_PEOPLE': 2, 'WHO': 'Mark', 'PLACE': 'Athens'} as arguments, would
 produce \"I see Mark and one other person in Athens.\" as output.

OR:

\"{NUM_FLOOR, selectordinal,
     one {Take the elevator to the #st floor.}
     two {Take the elevator to the #nd floor.}
     few {Take the elevator to the #rd floor.}
     other {Take the elevator to the #th floor.}}\"

 Providing {'NUM_FLOOR': 22} as arguments would produce:
 \"Take the elevator to the 22nd floor\"

Message templates are ICU message pattern. http://userguide.icu-project.org/formatparse/messages

This function is a wrapper for goog.i18n.MessageFormat, supporting a subset of the ICU MessageFormatSyntax.
"
     ([icu-pattern] (partial message (new MessageFormat icu-pattern)))
     ([formatter args-map] (.format formatter (clj->js args-map)))))

#?(:cljs
   (def DATE-FORMATS
     {:FULL-DATE       goog.i18n.DateTimeFormat.Format/FULL_DATE
      :LONG-DATE       goog.i18n.DateTimeFormat.Format/LONG_DATE,
      :MEDIUM-DATE     goog.i18n.DateTimeFormat.Format/MEDIUM_DATE,
      :SHORT-DATE      goog.i18n.DateTimeFormat.Format/SHORT_DATE
      :FULL-TIME       goog.i18n.DateTimeFormat.Format/FULL_TIME
      :LONG-TIME       goog.i18n.DateTimeFormat.Format/LONG_TIME
      :MEDIUM-TIME     goog.i18n.DateTimeFormat.Format/MEDIUM_TIME
      :SHORT-TIME      goog.i18n.DateTimeFormat.Format/SHORT_TIME
      :FULL-DATETIME   goog.i18n.DateTimeFormat.Format/FULL_DATETIME
      :LONG-DATETIME   goog.i18n.DateTimeFormat.Format/FULL_DATE
      :MEDIUM-DATETIME goog.i18n.DateTimeFormat.Format/MEDIUM_DATETIME
      :SHORT-DATETIME  goog.i18n.DateTimeFormat.Format/SHORT_DATETIME}
))

#?(:cljs
   (defn date
     ([pattern] (partial date (new DateTimeFormat (or (DATE-FORMATS pattern) pattern))))
     ([formatter date] (.format formatter date))))
