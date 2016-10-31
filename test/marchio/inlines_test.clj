(ns marchio.inlines-test
  (:require
    [clojure.test :refer [use-fixtures deftest testing is]]
    [marchio.inlines :as inlines]
    [marchio.ast :refer [new-node]])
  (:import (marchio.ast Node)))

(def n (new-node :paragraph))

(deftest parse-test
  (testing "Emphasis parsing 1"
    (is (instance? Node (inlines/parse-text "*foo bar*" n))))
  (testing "Emphasis parsing 2"
    (is (instance? Node (inlines/parse-text "*foo bar" n))))
  (testing "Emphasis parsing 3"
    (is (instance? Node (inlines/parse-text "**foo bar**" n))))
  (testing "Emphasis parsing 4"
    (is (instance? Node (inlines/parse-text "**foo** bar*" n))))
  (testing "Emphasis parsing 5"
    (is (instance? Node (inlines/parse-text "***foo** bar*" n))))
  (testing "Emphasis parsing 6"
    (is (instance? Node (inlines/parse-text "foo** bar*" n)))))