(ns dynamodb-expression.core
  "DynamoDB update expression construction functions"
  (:refer-clojure :rename {remove core-remove set core-set})
  (:require [clojure.string :as st]))

(defn- sanitize-placeholder [ph]
  (st/replace ph #"[^0-9a-zA-Z_]" "_"))

(defn update-expr
  "Initialise a set of DynamoDB updates against the given key."
  [k]
  {:ops []
   :key k})

(defn- field->str [f]
  (if (or (keyword? f) (symbol? f))
    (name f)
    (str f)))

(defn- replace-last-in-vec [v el]
  (assoc v (dec (count v)) el))

(defn- collapse-numbers-backwards [field-path & [prefix postfix]]
  (reduce
   #(if (number? %2)
      (replace-last-in-vec %1 (str (last %1) (or prefix "[") %2 (or postfix "]")))
      (conj %1 (field->str %2)))
   []
   field-path))

(defn- field-path->str
  ([field]
   (field-path->str "_" field))
  ([join-str field]
   (st/join join-str (map field->str field))))

(defn- new-op [op field value expr-part-fn & [related-symbols]]
  (let [f      (if (sequential? field) field [field])
        f-nums (collapse-numbers-backwards f "_" "")
        sym    (gensym (str (field->str (last f-nums)) "_"))
        path   (replace-last-in-vec f-nums (sanitize-placeholder sym))
        ->sym  (if related-symbols #(related-symbols % %) identity)
        o      {:op        op
                :field     (if (sequential? field)
                             (last (collapse-numbers-backwards field "[" "]"))
                             (field->str field))
                :sym       sym
                :val       value
                :expr-name (str "#n" (field-path->str path))
                :expr-val  (str ":v" (field-path->str path))
                :expr-path (->> path
                                (reductions #(str %1 (when %1 "_") %2) nil)
                                (map ->sym)
                                (core-remove nil?)
                                (map #(str "#n" %))
                                (field-path->str "."))}]
    (if expr-part-fn
      (assoc o :expr-part (expr-part-fn o))
      o)))

(defn- include-op
  ([expr op]
   (update-in expr [:ops] conj op))
  ([expr op field value expr-part-fn]
   (if (sequential? field)
     (let [field-path (collapse-numbers-backwards field)
           other-ops (->> (reductions conj [] (butlast field-path))
                          (core-remove empty?)
                          (map #(new-op op % nil nil)))
           related-symbols (into {} (map (juxt :field :sym) other-ops))]
       (->> other-ops
            (concat [(new-op op field value expr-part-fn related-symbols)])
            (reduce include-op expr)))
     (include-op expr (new-op op field value expr-part-fn)))))

(defn add
  "Add a DynamoDB ADD clause for one or more fields"
  [expr field value]
  (include-op expr :add field value #(str (:expr-path %) " " (:expr-val %))))

(def ^:private operator->str {'+ "+"
                              '- "-"
                              +  "+"
                              -  "-"
                              :+ "+"
                              :- "-"})

(defn set
  "Add a DynamoDB SET clause for one or more fields."
  ([expr field value]
   (include-op expr :set field value
               #(str (:expr-path %) " = " (:expr-val %))))
  ([expr field op value]
   (include-op expr :set field value
               #(str (:expr-path %) " = " (:expr-path %) " " (operator->str op op) " " (:expr-val %))))
  ([expr field other-field op value]
   (let [other-op (new-op :set other-field nil nil)]
     (-> expr
         (include-op :set field value
                     #(str (:expr-path %) " = " (:expr-path other-op) " " (operator->str op op) " " (:expr-val %)))
         (include-op other-op)))))

(defn delete
  "Add a DynamoDB DELETE clause for one or more fields."
  [expr field value]
  (include-op expr :delete field value #(str (:expr-path %) " " (:expr-val %))))

(defn remove
  "Add a DynamoDB REMOVE clause for one or more fields."
  [expr field]
  (include-op expr :remove field nil :expr-path))

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
  ;; (clojure.test/is (= [] ops))
  (->> ops
       (map (juxt name-or-value k))
       (core-remove #(some nil? %))
       (into {})
       not-empty))

(defn expr
  "Transform these operations into DynamoDB syntax."
  [{:keys [key ops] :as expr}]
  (let [expression-attribute-names (attr-map :expr-name :field ops)
        expression-attribute-values (attr-map :expr-val :val ops)]
    (cond-> {:update-expression (build-expression ops)
             :key key}
      expression-attribute-names  (assoc :expression-attribute-names  expression-attribute-names)
      expression-attribute-values (assoc :expression-attribute-values expression-attribute-values))))
