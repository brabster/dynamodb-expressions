(ns dynamodb-expression.core
  (:require [clojure.string :as st]))

(defn sanitize-placeholder [ph]
  (st/replace ph #"[^0-9a-zA-Z_]" "_"))

(defn update-expr []
  {:ops []})

(defn- new-op [field val]
  (let [field     (name field)
        sym       (sanitize-placeholder (str (gensym (str field "_"))))
        expr-name (str "#n" sym)
        expr-val  (str ":v" sym)]
    {:field     field
     :arg       val
     :sym       sym
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

(defn set
  ([expr field val]
   (let [{:keys [expr-name expr-val] :as op} (new-op field val)]
     (include-op expr :set op (str expr-name " = " expr-val))))
  ([expr field operator val]
   (let [{:keys [expr-name expr-val] :as op} (new-op field val)]
     (include-op expr :set op (str expr-name " = " expr-name " " operator " " expr-val))))
  ([expr field other-field operator val]
   (let [{:keys [expr-name expr-val] :as op} (new-op field val)
         other-op (new-op other-field nil)]
     (-> expr
         (include-op :set op (str expr-name " = " (:expr-name other-op) " " operator " " expr-val))
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
       (group-by :op)
       (into (sorted-map))
       (reduce (fn [ex [op ops]]
                 (prn '>> ops)
                 (->> ops
                      (map :expr-part)
                      (st/join ", ")
                      (str ex (when ex " ") (st/upper-case (name op)) " ")))
               nil)))

(defn- attr-map [name-or-value key ops]
  (->> ops
       (map (juxt name-or-value key))
       ;; (remove #(some nil? %))
       (into {})))

(defn expr [{:keys [ops] :as expr}]
  {:update-expression (build-expression ops)
   :expression-attribute-names (attr-map :expr-name :field ops)
   :expression-attribute-values (attr-map :expr-val :arg ops)})
