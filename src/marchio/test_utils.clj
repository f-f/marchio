(ns marchio.test-utils
  (:require
    [clojure.java.shell :refer [sh]]
    [clojure.data.xml :as xml]
    [clojure.walk :refer [postwalk]]
    [cheshire.core :refer [parse-string]]
    [taoensso.truss :refer [have]]
    [marchio.render :refer [md->html]]))

(defn load-spec-tests
  "Read spec tests, output a list of test maps."
  []
  (-> (sh "python3" "spec/test/spec_tests.py"
          "--spec" "spec/spec.txt"
          "--dump-tests")
      (:out)
      (parse-string true)))

;; Heart of the AST is the Node
(defrecord Node [type attrs children])

(defn cmark->marchio
  "Convert from cmark xml to marchio AST."
  [ast]
  (->> ast
       (postwalk
         (fn [el]
           (if (= (type el) clojure.data.xml.node.Element)
             (->Node (keyword (name (:tag el)))
                     (:attrs el)
                     (:content el))
             el)))))

(defn get-cmark-ast
  "Parse markdown with cmark, return marchio AST"
  [markdown]
  (xml/declare-ns "marchio" "http://commonmark.org/xml/1.0")
  (-> (sh "cmark" "--to" "xml" :in markdown)
      (:out)
      (xml/parse-str)
      (cmark->marchio)))

(defn validate!
  "Validate a single spec test markdown->html."
  [test]
  (have #(= (:html test)
            (md->html (:markdown test)))
        test))

(defn run-tests
  "Run all tests."
  [tests]
  (->> tests
       (mapv validate!)))