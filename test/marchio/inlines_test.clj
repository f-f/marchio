(ns marchio.inlines-test
  (:require
    [clojure.test :refer [use-fixtures deftest testing is]]
    [marchio.inlines :as inlines]
    [marchio.ast :refer [new-node]])
  (:import (marchio.ast Node)))

(deftest parse-test
  (testing "Emphasis parsing 1"
    (is (vector? (inlines/parse-line ["*foo bar*"]))))
  (testing "Emphasis parsing 2"
    (is (vector? (inlines/parse-line ["*foo bar"]))))
  (testing "Emphasis parsing 3"
    (is (vector? (inlines/parse-line ["**foo bar**"]))))
  (testing "Emphasis parsing 4"
    (is (vector? (inlines/parse-line ["**foo** bar*"]))))
  (testing "Emphasis parsing 5"
    (is (vector? (inlines/parse-line ["***foo** bar*"]))))
  (testing "Emphasis parsing 6"
    (is (vector? (inlines/parse-line ["foo** bar*"])))))