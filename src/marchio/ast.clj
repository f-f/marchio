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

(defn node-zipper
  "Returns a zipper for Block elements, given a root element"
  [root]
  (z/zipper (complement string?)
            (comp seq :content)
            (fn [node children]
              (assoc node :content (and children (apply vector children))))
            root))