(ns marchio.render
  (:require
    [marchio.ast :refer [parse-ast ast->hiccup]]
    [hiccup.core :as hiccup]))

(defn md->html
  "Converts a markdown formatted string to an HTML formatted string.
  Throws an Exception on syntax error."
  [text]
  (-> text
      (parse-ast)     ;; Parse the input text to AST
      (ast->hiccup)   ;; Convert the AST to Hiccup vectors
      (hiccup/html))) ;; Convert the Hiccup to HTML