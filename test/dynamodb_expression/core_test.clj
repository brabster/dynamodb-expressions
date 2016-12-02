(ns dynamodb-expression.core-test
  (:require [clojure.test :refer [deftest use-fixtures is are testing]]
            [dynamodb-expression.core :as dx]
            [dynamodb-expression.grammar-test :as g]))

(use-fixtures :each
  (fn [f]
    (let [cnt (atom 0)]
      (with-redefs [gensym (fn [& [prefix]]
                             (str prefix "G__" (swap! cnt inc)))]
        (f)))))

(defn is-expr= [expected-expr generated-expr]
  (is (vector? (g/parse expected-expr)) (str "Expected expression not valid : " expected-expr))
  (is (vector? (g/parse generated-expr)) (str "Generated expression not valid : " generated-expr))
  (is (= expected-expr generated-expr) (str "Unexpected expression" generated-expr)))

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
      (is-expr= "ADD #nfoo_bar_G__1 :vfoo_bar_G__1, #nbar_baz_G__2 :vbar_baz_G__2, #n_goof__G__3 :v_goof__G__3"
                update-expression))))

(deftest set-and-add-test
  (testing "Another basic integration test"
    (let [x 4
          {:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr {:id "12"})
                       (dx/set :something x)
                       (dx/add :something-else (* x 8))
                       (dx/add :fish 12)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= {"#nsomething_G__1" "something"
              "#nsomething_else_G__2" "something-else"
              "#nfish_G__3" "fish"}
             expression-attribute-names))
      (is (= {":vsomething_G__1" 4
              ":vsomething_else_G__2" 32
              ":vfish_G__3" 12}
             expression-attribute-values))
      (is-expr= update-expression "SET #nsomething_G__1 = :vsomething_G__1 ADD #nsomething_else_G__2 :vsomething_else_G__2, #nfish_G__3 :vfish_G__3"))))

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
              "#nsomething_G__2" "something"
              "#nsomething_else_G__3" "something-else"}
             expression-attribute-names))
      (is (= {":vsomething_G__2" 4
              ":vfish_G__1" 33}
             expression-attribute-values))
      (is-expr= "SET #nfish_G__1 = #nfish_G__1 + :vfish_G__1, #nsomething_G__2 = #nsomething_else_G__3 + :vsomething_G__2" update-expression))))

(deftest delete-test
  (testing "Yet another basic integration test"
    (let [{:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr {:id "12"})
                       (dx/delete :something "value")
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= {"#nsomething_G__1" "something"} expression-attribute-names))
      (is (= {":vsomething_G__1" "value"} expression-attribute-values))
      (is-expr= "DELETE #nsomething_G__1 :vsomething_G__1" update-expression))))

(deftest remove-test
  (testing "Yet another basic integration test"
    (let [{:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr {:id "12"})
                       (dx/remove :something)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= {"#nsomething_G__1" "something"} expression-attribute-names))
      (is (= {":vsomething_G__1" nil} expression-attribute-values))
      (is-expr= "REMOVE #nsomething_G__1" update-expression))))

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
        (is (= {"#nsomething_G__1" "something"
                "#nelse_G__2"      "else"
                "#nnew_G__3"       "new"
                "#nfish_G__4"      "fish"}
               expression-attribute-names)))
      (testing "values"
        (is (= {":vsomething_else_G__1" 12
                ":vsomething_new_G__2"  "munge"
                ":vfish_0_G__3"         21} expression-attribute-values)))
      (testing "expression"
        (is-expr= "ADD #nsomething_G__1.#nelse_G__2 :velse_G__2, #nsomething_G__1.#nnew_G__3 :vnew_G__3, #nfish_G__4 :vfish_G__4" update-expression)))))
