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
        (number? f)                              (str f)
        :default                                 (str f)))

(defn- new-op [field val]
  (let [field (if (sequential? field)
                (st/join "_" (map field->str field))
                (field->str field))
        sym          (sanitize-placeholder (str (gensym (str field "_"))))
        expr-name    (str "#n" sym)
        expr-val     (str ":v" sym)]
    {:field     field
     :val       val
     :expr-name expr-name
     :expr-val  expr-val}))

(defn- include-op [expr op-keywd op expr-part]
  (update-in expr [:ops] conj
             (merge op
                    {:op        op-keywd
                     :expr-part expr-part})))

(defn add [expr field val]
  (let [{:keys [expr-name expr-val] :as op} (new-op field val)]
    (include-op expr :add op (str expr-name " " expr-val))))

(def ^:private operator->str {'+ "+"
                              '- "-"
                              +  "+"
                              -  "-"
                              :+ "+"
                              :- "-"})

(defn set
  ([expr field val]
   (let [{:keys [expr-name expr-val] :as op} (new-op field val)]
     (include-op expr :set op (str expr-name " = " expr-val))))
  ([expr field operator val]
   (let [{:keys [expr-name expr-val] :as op} (new-op field val)]
     (include-op expr :set op (str expr-name " = " expr-name " " (operator->str operator operator) " " expr-val))))
  ([expr field other-field operator val]
   (let [{:keys [expr-name expr-val] :as op} (new-op field val)
         other-op (new-op other-field nil)]
     (-> expr
         (include-op :set op (str expr-name " = " (:expr-name other-op) " " (operator->str operator operator) " " expr-val))
         (update-in [:ops] conj
                    (-> other-op
                        (select-keys [:expr-name :field])
                        (assoc :op :set)))))))

(defn delete [expr field val]
  (let [{:keys [expr-name expr-val] :as op} (new-op field val)]
    (include-op expr :delete op (str expr-name " " expr-val))))

(defn remove [expr field]
  (let [{:keys [expr-name expr-val] :as op} (new-op field nil)]
    (include-op expr :remove op expr-name)))

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
       (core-remove #(-> % first nil?))
       (into {})))

(defn expr [{:keys [key ops] :as expr}]
  {:update-expression (build-expression ops)
   :expression-attribute-names (attr-map :expr-name :field ops)
   :expression-attribute-values (attr-map :expr-val :val ops)
   :key key})
