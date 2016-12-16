(ns dynamodb-expression.integration-test
  (:require  [dynamodb-expression.core :as dx]
             [clojure.test :refer :all]
             [docker.fixture :as docker]
             [amazonica.aws.dynamodbv2 :as ddb]
             [amazonica.core :refer [defcredential]]))

(def port (docker/rand-port))
(def host (docker/host))
(def creds {:access-key "foo" :secret-key "bar" :endpoint (str "http://" host ":" port)})
(def table-name "foo")

(def dynamodb-container
  (docker/new-fixture
   {:cmd ["docker" "run" "-d" "-p" (str port ":" port) "tray/dynamodb-local"
          "-inMemory" "-port" port]
    :sleep 500}))

(defn table [name hash]
  (fn [f]
    (println "Creating table" name)
    (ddb/create-table creds
                      :table-name name
                      :key-schema [{:attribute-name (:name hash) :key-type "HASH"}]
                      :attribute-definitions [{:attribute-name (:name hash) :attribute-type (:type hash)}]
                      :provisioned-throughput {:read-capacity-units 1
                                               :write-capacity-units 1})
    (f)
    (println "Deleting table" name)
    (ddb/delete-table creds :table-name name)))

(use-fixtures :once dynamodb-container (table table-name {:name "id" :type "S"}))


(deftest basic-ops-test
  (testing "set and add work"
    (is (= {:item {:id "1" :bar 0 :foo 1}}
           (do
             (ddb/update-item creds
                              (->
                               (dx/update-expr {})
                               (dx/add :foo 1)
                               (dx/set :bar 0)
                               (dx/expr)
                               (assoc :table-name table-name)
                               (assoc :key {:id "1"})))
             (ddb/get-item creds
                           :table-name table-name
                           :key {:id "1"})))))

  (testing "remove works"
    (is (= {:item {:id "2"}}
           (do
             (ddb/update-item creds
                              (->
                               (dx/update-expr {})
                               (dx/set :bar 0)
                               (dx/expr)
                               (assoc :table-name table-name)
                               (assoc :key {:id "2"})))
             (ddb/update-item creds
                              (-> (dx/update-expr {})
                                  (dx/remove :bar)
                                  (dx/expr)
                                  (assoc :table-name table-name)
                                  (assoc :key {:id "2"})))
             (ddb/get-item creds
                           :table-name table-name
                           :key {:id "2"})))))

  #_(testing "delete works"
      (let [delete-expr (->
                         (dx/update-expr {})
                         (dx/set :bar #{"foo"})
                         (dx/expr)
                         (assoc :table-name table-name)
                         (assoc :key {:id "3"}))]
        (prn delete-expr)
        (is (= {:item {:id "3" :things []}}
               (do
                 (ddb/update-item creds delete-expr)
                 (ddb/update-item creds
                                  (-> (dx/update-expr {})
                                      (dx/delete :bar "foo")
                                      (dx/expr)
                                      (assoc :table-name table-name)
                                      (assoc :key {:id "3"})))
                 (ddb/get-item creds
                               :table-name table-name
                               :key {:id "3"})))))))
