(ns dynamodb-expression.core
  (:require [clojure.string :as st]))

(defn update-expr []
  {:ops []})

(defn add [expr field val]
  (let [field     (name field)
        sym       (sanitize-placeholder (str (gensym (str field "_"))))
        expr-name (str "#" sym)
        expr-val  (str ":" sym)]
    (update-in expr [:ops] conj {:op        :add
                                 :field     field
                                 :arg       val
                                 :sym       sym
                                 :expr-name expr-name
                                 :expr-val  expr-val
                                 :expr-part (str expr-name " " expr-val)})))

(defn- build-expression [ops]
  (->> ops
       (group-by :op)
       (reduce (fn [ex [op ops]]
                 (->> ops
                      (map :expr-part)
                      (st/join ", ")
                      (str ex (st/upper-case (name op)) " ")))
               nil)))

(defn- attr-map [name-or-value key ops]
  (->> ops
       (map (juxt name-or-value key))
       (into {})))

(defn expr [{:keys [ops] :as expr}]
  {:update-expression (build-expression ops)
   :expression-attribute-names (attr-map :expr-name :field ops)
   :expression-attribute-values (attr-map :expr-val :arg ops)})
