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

(deftest a-test
  (testing "A basic integration test"
    (let [x 4
          {:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr)
                       (dx/add "foo.bar" x)
                       (dx/add :bar.baz 8)
                       (dx/add "-goof-" 76)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= expression-attribute-names {"#nfoo_bar_G__1" "foo.bar"
                                         "#nbar_baz_G__2" "bar.baz"
                                         "#n_goof__G__3" "-goof-"}))
      (is (= expression-attribute-values {":vfoo_bar_G__1" 4
                                          ":vbar_baz_G__2" 8
                                          ":v_goof__G__3" 76}))
      (is (= update-expression "ADD #nfoo_bar_G__1 :vfoo_bar_G__1, #nbar_baz_G__2 :vbar_baz_G__2, #n_goof__G__3 :v_goof__G__3"))
      (is (vector? parsed-exp)))))

(deftest set-test
  (testing "Another basic integration test"
    (let [x 4
          {:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr)
                       (dx/set :something x)
                       (dx/add :something-else (* x 8))
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= expression-attribute-names {"#nsomething_G__1" "something"
                                         "#nsomething_else_G__2" "something-else"}))
      (is (= expression-attribute-values {":vsomething_G__1" 4
                                          ":vsomething_else_G__2" 32}))
      (is (vector? (g/parse "ADD #nsomething_else_G__2 :vsomething_else_G__2 SET #nsomething_G__1 = :vsomething_G__1")))
      (is (= update-expression "ADD #nsomething_else_G__2 :vsomething_else_G__2 SET #nsomething_G__1 = :vsomething_G__1"))
      (is (vector? parsed-exp)))))

(deftest delete-test
  (testing "Yet another basic integration test"
    (let [{:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr)
                       (dx/delete :something "value")
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= expression-attribute-names {"#nsomething_G__1" "something"}))
      (is (= expression-attribute-values {":vsomething_G__1" "value"}))
      (is (vector? (g/parse "DELETE #nsomething_G__1 :vsomething_G__1")))
      (is (= update-expression "DELETE #nsomething_G__1 :vsomething_G__1"))
      (is (vector? parsed-exp)))))

(deftest remove-test
  (testing "Yet another basic integration test"
    (let [{:keys [update-expression expression-attribute-names expression-attribute-values]
           :as ex} (-> (dx/update-expr)
                       (dx/remove :something)
                       dx/expr)
          parsed-exp (g/parse update-expression)]
      (is (= expression-attribute-names {"#nsomething_G__1" "something"}))
      (is (= expression-attribute-values {":vsomething_G__1" nil}))
      (is (vector? (g/parse "REMOVE #nsomething_G__1")))
      (is (= update-expression "REMOVE #nsomething_G__1"))
      (is (vector? parsed-exp)))))
