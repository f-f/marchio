(ns marchio.ast
  (:require
    [clojure.zip :as z]))

;; Heart of the AST is the Node
(defrecord Node [tag attrs content])

(defn new-node
  "Node entity constructors."
  ([tag]               (new-node tag {} []))
  ([tag       content] (new-node tag {} [content]))
  ([tag attrs content] (->Node tag attrs content)))

(defn new-tree
  "Returns a new tree zipper for the AST."
  []
  (z/zipper
    (complement string?)
    (comp seq :content)
    (fn [node children]
      (assoc node :content (and children (apply vector children))))
    (new-node :document {:open? true} [])))

;; Zippers are awesome:
;; http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/
;; http://josf.info/blog/2014/03/28/clojure-zippers-structure-editing-with-your-mind/
;; http://josf.info/blog/2014/04/14/seqs-of-clojure-zippers/

;; TODO: implement `find-open-blocks`
;; http://spec.commonmark.org/0.27/#phase-1-block-structure

(comment
  (-> (new-tree)
      (z/append-child (new-node :text "test"))))

