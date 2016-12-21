(ns marchio.test
  (:require
    [clojure.java.shell :refer [sh]]
    [clojure.data.xml :as xml]
    [clojure.walk :refer [postwalk]]
    [clojure.test :refer [use-fixtures deftest testing is]]
    [cheshire.core :refer [parse-string generate-string]]
    [marchio.render :as render]
    [marchio.ast :refer [new-node compact-text-nodes]]
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
                       (->> el
                            (:content)
                            (into [])))
             el)))))

(defn get-cmark-ast
  "Parse markdown with cmark, return marchio AST."
  [markdown]
  (xml/declare-ns "commonmark" "http://commonmark.org/xml/1.0")
  (-> (sh "cmark" "--to" "xml" "--normalize" :in markdown)
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
          (->> spec-tests
               (filter
                 #(contains?
                    #{"Inlines"
                      "Backslash escapes"
                      "Entity and numeric character references"
                      "Code spans"
                      "Emphasis and strong emphasis"
                      ;"Links"
                      ;"Images"
                      "Autolinks"
                      "Raw HTML"
                      "Hard line breaks"
                      "Soft line breaks"
                      "Textual content"}

                      ;"Precedence"
                      ;"ATX headings"
                      ;"Fenced code blocks"
                      ;"HTML blocks"
                      ;"Indented code blocks"
                      ;"Paragraphs"
                      ;"Link reference definitions"
                      ;"Tabs"
                      ;"Thematic breaks"
                      ;"List items"
                      ;"Blank lines"
                      ;"Lists"
                      ;"Setext headings"
                      ;"Block quotes"
                    (:section %)))
               ;; Filter even more on tests that fail
               (remove
                 #(contains? ; Reason to exclude them:
                    #{288 ; Tabs
                      293 ; Code block
                      294 ; Code block
                      295 ; Link
                      296 ; HTML block
                      297 ; Link
                      298 ; Link
                      299 ; Code block
                      306 ; HTML block
                      307 ; Link
                      308 ; Link
                      309 ; Code block
                      311 ; Code block
                      325 ; Link
                      381 ; Link
                      393 ; Link
                      396 ; Link
                      407 ; Link
                      447 ; Link
                      448 ; Link
                      454 ; Link
                      455 ; Link
                      595 ; Block parsing
                      614 ; Block space processing
                      615 ; Headings
                      616 ; Headings
                      618}; Block alignment
                    (:example %))))]
               ;(filter #(= 290 (:example %))))]
    (testing (str "AST: " section ", " example "\nText: " markdown)
      (is (= (get-cmark-ast markdown)
             (parse/text->ast markdown))))))
    ;(testing (str "HTML: " section ", " example)
    ;  (is (= html (render/md->html markdown))))))