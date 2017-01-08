(ns dynamodb-expression.core-test
  (:require [clojure.test :refer [deftest use-fixtures is are testing]]
            [dynamodb-expression.core :as dx]
            [dynamodb-expression.grammar-test :as g]))

(defn strip-newlines [s] (clojure.string/replace s #"\n" ""))

(use-fixtures :each
  (fn [f]
    (let [cnt (atom 0)]
      (with-redefs [gensym (fn [& [prefix]]
                             (str prefix "G__" (swap! cnt inc)))]
        (f)))))

(defn invalid-expr? [expected-expr generated-expr]
  (cond
    (not (g/parsed? (g/parse expected-expr)))
    ["Expected expression not valid : " expected-expr (g/parse expected-expr)]

    (not (g/parsed? (g/parse generated-expr)))
    ["Generated expression not valid : " generated-expr (g/parse generated-expr)]

    (not= expected-expr generated-expr)
    ["Unexpected expression" generated-expr "expected" expected-expr]

    :else
    false))

(deftest a-test
  (testing "A basic integration test"
    (let [x 4
          id {:id "12"}
          {:keys [update-expression expression-attribute-names expression-attribute-values key]
           :as ex} (-> (dx/update-expr id)
                       (dx/add "foo.bar" x)
                       (dx/add :bar.baz 8)
                       (dx/add "-goof-" 76)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= id key))
      (is (= {"#nfoo_bar_G__1" "foo.bar"
              "#nbar_baz_G__2" "bar.baz"
              "#n_goof__G__3" "-goof-"}
             expression-attribute-names))
      (is (= {":vfoo_bar_G__1" 4
              ":vbar_baz_G__2" 8
              ":v_goof__G__3" 76}
             expression-attribute-values))
      (is (not
           (invalid-expr?
            "ADD #nfoo_bar_G__1 :vfoo_bar_G__1, #nbar_baz_G__2 :vbar_baz_G__2, #n_goof__G__3 :v_goof__G__3"
            update-expression))))))

(deftest set-and-add-test
  (testing "Another basic integration test"
    (let [x 4
          {:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr {:id "12"})
                       (dx/set :x x)
                       (dx/add :8x (* x 8))
                       (dx/add :fish 12)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= {"#nx_G__1" "x"
              "#n8x_G__2" "8x"
              "#nfish_G__3" "fish"}
             expression-attribute-names))
      (is (= {":vx_G__1" 4
              ":v8x_G__2" 32
              ":vfish_G__3" 12}
             expression-attribute-values))
      (is (not
           (invalid-expr? update-expression
                          "SET #nx_G__1 = :vx_G__1 ADD #n8x_G__2 :v8x_G__2, #nfish_G__3 :vfish_G__3"))))))

(deftest set-test
  (testing "Another basic integration test"
    (let [x 4
          {:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr {:id "12"})
                       (dx/set :fish + 33)
                       (dx/set :something :something-else "+" 4)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= {"#nfish_G__1" "fish"
              "#nsomething_G__3" "something"
              "#nsomething_else_G__2" "something-else"}
             expression-attribute-names))
      (is (= {":vsomething_G__3" 4
              ":vfish_G__1" 33}
             expression-attribute-values))
      (is (not
           (invalid-expr? (str "SET #nfish_G__1 = #nfish_G__1 + :vfish_G__1, "
                               "#nsomething_G__3 = #nsomething_else_G__2 + :vsomething_G__3")
                          update-expression))))))

(deftest delete-test
  (testing "Yet another basic integration test"
    (let [{:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr {:id "12"})
                       (dx/delete :something "value")
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= {"#nsomething_G__1" "something"} expression-attribute-names))
      (is (= {":vsomething_G__1" "value"} expression-attribute-values))
      (is (not
           (invalid-expr? "DELETE #nsomething_G__1 :vsomething_G__1" update-expression))))))

(deftest remove-test
  (testing "Yet another basic integration test"
    (let [{:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr {:id "12"})
                       (dx/remove :something)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= {"#nsomething_G__1" "something"} expression-attribute-names))
      (is (= nil expression-attribute-values))
      (is (not
           (invalid-expr? "REMOVE #nsomething_G__1" update-expression))))))

(deftest path-test
  (testing "Yet another basic integration test"
    (let [{:keys [update-expression expression-attribute-names expression-attribute-values]
           :as   ex} (-> (dx/update-expr {:id "12"})
                         (dx/add [:something :else] 12)
                         (dx/add [:something :new] "munge")
                         (dx/add [:fish 0] 21)
                         dx/expr)
          parsed-exp (g/parse update-expression)]
      (testing "names"
        (is (= {"#nsomething_else_G__2" "else"
                "#nsomething_G__1"      "something"
                "#nsomething_new_G__4"  "new"
                "#nsomething_G__3"      "something"
                "#nfish_0_G__5"         "fish[0]"}
               expression-attribute-names)))
      (testing "values"
        (is (= {":vsomething_else_G__2" 12
                ":vsomething_new_G__4"  "munge"
                ":vfish_0_G__5"         21}
               expression-attribute-values)))
      (testing "expression"
        (is (not
             (invalid-expr? (str "ADD #nsomething_G__1.#nsomething_else_G__2 :vsomething_else_G__2, "
                                 "#nsomething_G__3.#nsomething_new_G__4 :vsomething_new_G__4, "
                                 "#nfish_0_G__5 :vfish_0_G__5")
                            update-expression))))
      (testing "set"
        (is (not
             (invalid-expr?
              "SET #nsomething_G__7.#nsomething_else_G__8 = #ndog_G__6 + :vsomething_else_G__8"
              (-> (dx/update-expr {:id "12"})
                  (dx/set [:something :else] :dog + 1)
                  dx/expr
                  :update-expression))))))))
