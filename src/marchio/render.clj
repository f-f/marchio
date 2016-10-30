(ns marchio.render
  (:require
    [marchio.ast :refer [text->ast ast->hiccup]]
    [hiccup.core :as hiccup]))

(defn md->html
  "Converts a markdown formatted string to an HTML formatted string.
  Throws an Exception on syntax error."
  [text]
  (-> text
      (text->ast)
      (ast->hiccup)
      (hiccup/html)))