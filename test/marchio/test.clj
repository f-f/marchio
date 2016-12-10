(ns marchio.test
  (:require
    [clojure.java.shell :refer [sh]]
    [clojure.data.xml :as xml]
    [clojure.walk :refer [postwalk]]
    [clojure.test :refer [use-fixtures deftest testing is]]
    [cheshire.core :refer [parse-string generate-string]]
    [marchio.render :as render]
    [marchio.ast :refer [new-node]]
    [marchio.parse :as parse])
  (:import (clojure.data.xml.node Element)))

(defn cmark->marchio
  "Convert from cmark xml to marchio AST."
  [ast]
  (->> ast
       (postwalk
         (fn [el] ;; if is here so that strings don't get wrapped
           (if (= (type el) Element)
             (new-node (keyword (name (:tag el)))
                       (:attrs el)
                       (into [] (:content el)))
             el)))))

(defn get-cmark-ast
  "Parse markdown with cmark, return marchio AST."
  [markdown]
  (xml/declare-ns "commonmark" "http://commonmark.org/xml/1.0")
  (-> (sh "cmark" "--to" "xml" :in markdown)
      (:out)
      (xml/parse-str)
      (cmark->marchio)))

(defn load-spec-tests
  "Read spec tests, output a list of test maps."
  []
  (-> (sh "python3" "spec/test/spec_tests.py"
          "--spec" "spec/spec.txt"
          "--dump-tests")
      (:out)
      (parse-string true)))

(defn spec-fixture [f]
  (def spec-tests (load-spec-tests))
  (f))

(use-fixtures :once spec-fixture)

(deftest ast-testing
  (doseq [{:keys [html markdown section example]}
          (filter
            #(contains?
               #{"Inlines"
                 "Hard line breaks"}
               (:section %))
            spec-tests)]
    (testing (str "AST: " section ", " example "\nText: " markdown)
      (is (= (get-cmark-ast markdown)
             (parse/text->ast markdown))))))
    ;(testing (str "HTML: " section ", " example)
    ;  (is (= html (render/md->html markdown))))))