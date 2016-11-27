(ns dynamodb-expression.core)

(defn sanitize-placeholder [ph] (clojure.string/replace ph #"[^0-9a-zA-Z_]" "_"))

(defn gen-placeholder [prefix name]
  (str prefix (sanitize-placeholder name) "_" (gensym)))

(defn generate-name-placeholder [name] (gen-placeholder "#" name))

(defn generate-value-placeholder [value] (gen-placeholder ":" value))

(defmulti resolve-expr-value #(class %))

(defmethod resolve-expr-value clojure.lang.Symbol [x] (deref (resolve x)))

(defmethod resolve-expr-value :default [x] x)

(defn handle-form [[name & parts]]
  (cond
    (= 'add name) (reduce
                   (fn [acc [path upd]]
                     (let [gpath (generate-name-placeholder path)
                           gupdate (generate-value-placeholder upd)]
                       (-> acc
                           (update :expression #(vec (conj % [gpath gupdate])))
                           (update :attr-names #(assoc % gpath path))
                           (update :attr-values #(assoc % gupdate (resolve-expr-value upd))))))
                   {:type :add}
                   parts)))

(defmulti render-update-expression (fn [args] (first args)))

(defmethod render-update-expression :default [[t _]] (throw (ex-info "Can't handle type" {:type t})))

(defmethod render-update-expression :add [[_ pairs]]
  (->> pairs
       (map #(clojure.string/join " " %))
       (clojure.string/join ", ")))

(defn type-kwd->type-expr [kwd] (-> kwd name clojure.string/upper-case))

(defn reduce-partial-exprs [partial-type partials]
  (->
   (reduce (fn [{:keys [update-expression expression-attribute-names expression-attribute-values]
                 :as acc}
                {:keys [type expression attr-names attr-values]}]
             {:update-expression (vec (conj update-expression (render-update-expression [partial-type expression])))
              :expression-attribute-names (merge expression-attribute-names attr-names)
              :expression-attribute-values (merge expression-attribute-values attr-values)})
           {}
           partials)
   (update :update-expression #(str (type-kwd->type-expr partial-type) " " (first %)))))

(defn expr [& forms]
  (->>
   forms
   (map handle-form)
   (group-by :type)
   (map (fn [[t partials]] (reduce-partial-exprs t partials)))))
