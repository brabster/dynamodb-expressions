(ns dynamodb-expression.core
  (:refer-clojure :rename {remove core-remove set core-set})
  (:require [clojure.string :as st]))

(defn- sanitize-placeholder [ph]
  (st/replace ph #"[^0-9a-zA-Z_]" "_"))

(defn update-expr [key]
  {:ops []
   :key key})

(defn- field->str [f]
  (if (or (keyword? f) (symbol? f))
    (name f)
    (str f)))

(defn- collapse-numbers-backwards [field-path]
  (reduce
   #(if (number? %2)
      (assoc %1 (dec (count %1)) (str (last %1) "[" %2 "]"))
      (conj %1 (field->str %2)))
   []
   field-path))

(defn field-path->str [field & [join-str]]
  (if (sequential? field)
    (st/join (or join-str "_") (map field->str field))
    (field->str field)))

(defn- new-op [op field val expr-part-fn]
  (let [f         (field-path->str field)
        sym       (sanitize-placeholder (str (gensym (str f "_"))))
        expr-name (str "#n"
                       (if (sequential? field)
                         (field-path->str field ".")
                         sym))
        expr-val  (str ":v" sym)
        o         {:op        op
                   :field     (if (sequential? field)
                                (last (collapse-numbers-backwards field))
                                (field->str field))
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
     (let [field-path (collapse-numbers-backwards field)]
       (->> (reductions conj [] (butlast field-path))
            (core-remove empty?)
            (map #(new-op op % nil nil))
            (concat [(new-op op field val expr-part-fn)])
            (reduce include-op expr)))
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

(defn- attr-map [name-or-value k ops]
  (->> ops
       (map (juxt name-or-value key))
       (core-remove #(some nil? %))
       (into {})))

(defn expr [{:keys [key ops] :as expr}]
  (let [expression-attribute-names (attr-map :expr-name :field ops)
        expression-attribute-values (attr-map :expr-val :val ops)]
    (cond-> {:update-expression (build-expression ops)
             :key key}
      expression-attribute-names  (assoc :expression-attribute-names  expression-attribute-names)
      expression-attribute-values (assoc :expression-attribute-values expression-attribute-values))))
