(ns dynamodb-expression.core
  (:refer-clojure :rename {remove core-remove})
  (:require [clojure.string :as st]))

(defn- sanitize-placeholder [ph]
  (st/replace ph #"[^0-9a-zA-Z_]" "_"))

(defn update-expr [key]
  {:ops []
   :key key})

(defn field->str [f]
  (cond (or (keyword? f) (symbol? f) (string? f)) (name f)
        (number? f)                               (str "[" f "]")
        :default                                  (str f)))

(defn- new-op [op field val expr-part-fn]
  (let [f         (if (sequential? field)
                    (st/join "_" (map field->str field))
                    (field->str field))
        sym       (sanitize-placeholder (str (gensym (str f "_"))))
        expr-name (str "#n" sym)
        expr-val  (str ":v" sym)
        o         {:op        op
                   :field     (field->str (if (sequential? field) (last field) field))
                   :val       val
                   :expr-name expr-name
                   :expr-val  expr-val}]
    (if expr-part-fn
      (assoc o :expr-part (expr-part-fn o))
      o)))

#_ (defn part-> [f & args]
     #(apply f % args))

(defn- include-op
  ([expr op]
   (update-in expr [:ops] conj op))
  ([expr op field val expr-part-fn]
   (if (sequential? field)
     (->> (reductions conj [] (butlast field))
          (core-remove empty?)
          (map #(new-op op % nil nil))
          (concat [(new-op op field val expr-part-fn)])
          (reduce include-op expr))
     (include-op expr (new-op op field val expr-part-fn)))))

(defn add [expr field val]
  (include-op expr :add field val #(str (:expr-name %) " " (:expr-val %))))

(def ^:private operator->str {'+ "+"
                              '- "-"
                              +  "+"
                              -  "-"
                              :+ "+"
                              :- "-"})

(defn set
  ([expr field val]
   (include-op expr :set field val #(str (:expr-name %) " = " (:expr-val %))))
  ([expr field operator val]
   (include-op expr :set field val #(str (:expr-name %) " = " (:expr-name %) " " (operator->str operator operator) " " (:expr-val %))))
  ([expr field other-field operator val]
   (let [other-op (new-op :set other-field nil nil)]
     (-> expr
         (include-op :set field val #(str (:expr-name %) " = " (:expr-name other-op) " " (operator->str operator operator) " " (:expr-val %)))
         (include-op other-op)))))

(defn delete [expr field val]
  (include-op expr :delete field val #(str (:expr-name %) " " (:expr-val %))))

(defn remove [expr field]
  (include-op expr :remove field nil :expr-name))

(defn- build-expression [ops]
  (->> ops
       (partition-by :op)
       (reduce (fn [ex [{:keys [op]} :as ops]]
                 (->> ops
                      (keep :expr-part)
                      (st/join ", ")
                      (str ex (when ex " ") (st/upper-case (name op)) " ")))
               nil)))

(defn- attr-map [name-or-value key ops]
  (->> ops
       (map (juxt name-or-value key))
       (core-remove #(some nil? %))
       (into {})))

(defn expr [{:keys [key ops] :as expr}]
  {:update-expression (build-expression ops)
   :expression-attribute-names (attr-map :expr-name :field ops)
   :expression-attribute-values (attr-map :expr-val :val ops)
   :key key})
