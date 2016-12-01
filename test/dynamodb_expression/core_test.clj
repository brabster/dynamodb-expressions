(ns dynamodb-expression.core-test
  (:require [clojure.test :refer :all]
            [dynamodb-expression.core :as dx]))

(deftest a-test
  (testing "A basic integration test"
    (let [cnt (atom 0)]
      (with-redefs [gensym (fn [& [prefix]]
                             (str prefix "G__" (swap! cnt inc)))]
        (let [x 4
              {:keys [update-expression expression-attribute-names expression-attribute-values]
               :as ex} (-> (dx/update-expr)
                           (dx/add "foo.bar" x)
                           (dx/add :bar.baz 8)
                           (dx/add "-goof-" 76)
                           dx/expr)]
          (is (= expression-attribute-names {"#foo_bar_G__1" "foo.bar",
                                             "#bar_baz_G__2" "bar.baz",
                                             "#_goof__G__3" "-goof-"}))
          (is (= expression-attribute-values {":foo_bar_G__1" 4,
                                              ":bar_baz_G__2" 8,
                                              ":_goof__G__3" 76}))
          (is (= update-expression "ADD #foo_bar_G__1 :foo_bar_G__1, #bar_baz_G__2 :bar_baz_G__2, #_goof__G__3 :_goof__G__3")))))))

