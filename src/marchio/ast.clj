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
  ([]     (new-tree (new-node :document {:open? true} [])))
  ([root] (z/zipper
            (complement string?)
            (comp seq :content)
            (fn [node children]
              (assoc node :content (and children (apply vector children))))
            root)))

;; Zippers are awesome:
;; http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/
;; http://josf.info/blog/2014/03/28/clojure-zippers-structure-editing-with-your-mind/
;; http://josf.info/blog/2014/04/14/seqs-of-clojure-zippers/

;; TODO: implement `find-open-blocks`
;; http://spec.commonmark.org/0.27/#phase-1-block-structure

(defn update-tree
  "Takes an ast and a function, and if (t node) is truthy applies the f to every
   node, recurring on the updated tree."
  [tree t f]
  (loop [loc (new-tree (z/root tree))]
    (if (z/end? loc)
      loc
      (recur (z/next (if (t (z/node loc))
                       (z/edit loc f)
                       loc))))))

(defn append-child
  "Simple alias for same function in zip."
  [tree new]
  (z/append-child tree new))

(defn compact-text-nodes
  "Given a list of text nodes, join together the contiguous ones."
  [nodes]
  (reduce (fn [new-vec {:keys [tag content] :as el}]
            (let [last-t   (last new-vec)
                  last-str (-> last-t :content first)]
              (if (and (= tag :text)
                       (= (:tag last-t) :text))
                (conj (pop new-vec)
                      (update-in (last new-vec)
                                 [:content 0]
                                 #(str % (first content))))
                (conj new-vec el))))
          []
          nodes))

(comment
  (-> (new-tree)
      (z/append-child (new-node :text "test"))
      (z/append-child (new-node :text "test2"))
      (update-tree
        string?
        #(do (println (type %) (string? %))
             %))))
