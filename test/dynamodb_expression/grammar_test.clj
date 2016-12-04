(ns dynamodb-expression.grammar-test
  (:require [instaparse.core :as insta]
            [clojure.test :refer :all]
            [dynamodb-expression.core :as dx]))

(def parse
  (insta/parser
   "S = update-expression (whitespace update-expression)*
    update-expression = ADD | SET | REMOVE | DELETE
    ADD = 'ADD' add-clause* last-add-clause
    last-add-clause = whitespace path whitespace value
    add-clause = last-add-clause comma

    SET = 'SET' set-clause* last-set-clause
    last-set-clause = whitespace path whitespace '=' whitespace (set-equal | set-modified)
    set-equal = value-placeholder
    set-modified = name-placeholder whitespace operator whitespace value-placeholder
    set-clause = last-set-clause comma

    REMOVE = 'REMOVE' remove-clause* last-remove-clause
    last-remove-clause = whitespace path
    remove-clause = last-remove-clause comma

    DELETE = 'DELETE' delete-clause* last-delete-clause
    last-delete-clause = whitespace path whitespace value-placeholder
    delete-clause = last-delete-clause comma

    operator = '+' | '-'
    path = name-placeholder | path-parts
    path-parts = path-part+ last-path-part
    last-path-part = name-placeholder
    path-part = name-placeholder dot
    name-placeholder = '#' legal-name | legal-name | list-item
    list-item = legal-name '[' #'[0-9]+' ']'
    legal-name = #'[a-zA-Z][a-zA-Z0-9_]*'
    value = value-placeholder
    value-placeholder = ':' legal-name
    whitespace = ' '
    comma = ','
    dot = '.'"))


(deftest dynamodb-expression-grammar-test
  (testing "AWS documentation examples parse"
    (is (vector? (parse "SET list[0] = :val1")))
    (is (vector? (parse "REMOVE #m.nestedField1, #m.nestedField2")))
    (is (vector? (parse "ADD aNumber :val2, anotherNumber :val3")))
    (is (vector? (parse "DELETE aSet :val4")))
    (is (vector? (parse "SET list[0] = :val1 REMOVE #m.nestedField1, #m.nestedField2 ADD aNumber :val2, anotherNumber :val3 DELETE aSet :val4")))
    (is (vector? (parse "SET Price = Price - :p")))
    (is (vector? (parse "REMOVE MyNumbers[1], MyNumbers[3]"))))

  (testing "Badly-formed examples don't parse"
    (testing "random word"
      (is (map? (parse "foo"))))
    (testing "extra comma between types of operation"
      (is (map? (parse "SET list[0] = :val1, REMOVE #m.nestedField1, #m.nestedField2"))))
    (testing "missing comma between two of same type of operation"
      (is (map? (parse "SET list[0] = :val1 REMOVE #m.nestedField1 #m.nestedField2"))))))
