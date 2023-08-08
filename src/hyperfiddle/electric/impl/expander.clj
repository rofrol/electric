(ns hyperfiddle.electric.impl.expander
  (:import [clojure.lang ISeq]))

(defn- fn-> [f a] (fn [o] (f o a)))

(declare expand-in-try)

(defn expand
  ([o] (expand o #{}))
  ([o ls]
   (cond
     (instance? ISeq o)
     (if (ls (first o))
       (list* (first o) (mapv (fn-> expand ls) (rest o)))
       (case (first o)
         ;; (ns ns* deftype* defrecord* var)

         (let* loop*) (let [[op bs & body] o
                            [bs2 ls2] (reduce
                                        (fn [[bs ls] [sym v]]
                                          [(conj bs sym (expand v ls)) (conj ls sym)])
                                        [[] ls]
                                        (partition-all 2 bs))]
                        (list* op bs2 (mapv (fn-> expand ls2) body)))

         (case) (let [[_ v & clauses] o
                      has-default-clause? (odd? (count clauses))
                      clauses2 (cond-> clauses has-default-clause? butlast)
                      xpand (fn-> expand ls)]
                  (list* 'case (xpand v)
                    (cond-> (into [] (comp (partition-all 2) (mapcat (fn [[match expr]] [match (xpand expr)])))
                              clauses2)
                      has-default-clause? (conj (xpand (last clauses))))))

         (quote) (second o)

         (fn*) (let [[?name arities] (if (symbol? (second o)) [(second o) (nnext o)] [nil (next o)])]
                 (doall (concat (if ?name (list 'fn* ?name) '(fn*))
                          (for [[syms & body] arities]
                            (list* syms (mapv (fn-> expand (into ls syms)) body))))))

         (letfn*) (let [[_ bs & body] o
                        ls2 (into ls (take-nth 2 bs))
                        xpand (fn-> expand ls2)]
                    (list* 'letfn*
                      (into [] (comp (partition-all 2) (mapcat (fn [[sym v]] [sym (xpand v)]))) bs)
                      (mapv xpand body)))

         (try) (list* 'try (mapv (fn-> expand-in-try ls) (rest o)))

         #_else
         (let [o2 (macroexpand-1 o)]
           (if (identical? o o2)
             (list* (first o) (mapv (fn-> expand ls) (rest o)))
             (recur o2 ls)))))

     (map-entry? o) (first {(expand (key o) ls) (expand (val o) ls)})
     (coll? o) (into (empty o) (map (fn-> expand ls) o))
     :else o)))

(defn expand-in-try [o ls]
  (cond
    (instance? ISeq o)
    (if (ls (first o))
      (list* (first o) (mapv (fn-> expand ls) (rest o)))
      (case (first o)
        (catch) (let [[_ typ sym & body] o, ls2 (conj ls sym)]
                  (list* 'catch typ sym (mapv (fn-> expand ls2) body)))
        #_else (expand o ls)))))

(comment
  (macroexpand-1 '(do 1 2))
  (macroexpand-1 '(if 1 2))
  (macroexpand '(fn [x] x))
  (let [let inc] (let 1))
  (macroexpand-1 '(-> 1 inc))
  (macroexpand-1 ''1)
  (type '[(-> 2 inc)])
  ;; how much static analysis will we do right now?
  ;; (new Foo 1) -> yes
  ;; (new (e/fn ...)) -> yes
  ;; (let [x Foo] (new x 1)) -> maybe
  ;; (new (first [Foo]) 1) -> no
  ;; the first 2 will be pareto efficient
  ;; e/fn is a macro and it already expands
  ;; all we lack now is resolving nodes (e/defs)
  (let [x Foo] (new x 1))
  (new (first [Foo]) 1)
  )
