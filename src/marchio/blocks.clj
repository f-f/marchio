(ns marchio.blocks
  (:require [blancas.kern.core :as k]
            [marchio.combinators :as c]
            [marchio.ast :refer [new-node new-tree]]
            [clojure.string :as str]))

;; TODO: conj is not good of course, we need to manipulate the zipper
;;       and append in the right place
;; TODO: replace `run` with `parse-data` -> faster!
(defn parse-line
  [tree line]
  (update tree
    :content
    conj
    (-> c/any
        (k/value line))))

(defn parse
  "AST Building, Phase 1: parse lines of text, and recursively build an AST
   with only block structure."
  [lines]
  (loop [tree (new-node :document)
         ls   lines]
    (if (empty? ls)
      tree;(close-blocks tree)
      (recur (parse-line tree (first ls))
             (rest ls)))))

(defn parse
  "Temporary replacement for testing inlines"
  [lines]
  (new-tree
    (new-node :document
      (new-node :paragraph (str/join "\n" lines)))))

(comment
  (defn close-blocks
    "Finalize every block in the tree. Close it and postprocess, e.g. creating
     string_content from strings, setting the 'tight' or 'loose' status of a list,
     and parsing the beginnings of paragraphs for reference definitions."
    [tree]
    (->> tree
         (postwalk
           (fn [el]
             (if (and (map? el))
               (-> el
                   (assoc :open? false))
               ;(finalize el)) ;; <------------ TODO
               el))))))