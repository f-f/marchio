(ns marchio.inlines.emph
  (:require
    [clojure.string :as string]
    [marchio.ast :as ast :refer [new-node]]
    [uncomplicate.fluokitten.core :refer [fmap fapply bind return fold foldmap]]
    [uncomplicate.fluokitten.jvm]
    [marchio.chars :as c]))

(defn append-char [node char]
  (update node :content
          (fn [children]
            (if-not (string? (last children))
              (conj (or children []) char)
              (assoc children (dec (count children))
                              (str (last children) char))))))





(def not-neg? (complement neg?))

(defn index-of
  "Returns the index of first el in coll if found, otherwise nil"
  [coll el]
  (let [index    (.indexOf coll el)]
    (when (not-neg? index)
      index)))

(defn index-of-next
  "Return the index of the next char in the chars types list"
  [char chars-types]
  (when-let [index (-> chars-types (subvec 1) (index-of char))]
    (inc index)))

(defn delimiter?
  "Takes a char, returns true if it's a delimiter"
  [char]
  (some #{char} [c/Underscore c/Asterisk c/Singlequote c/Doublequote]))

(defn next-delimiter
  "Takes an index and a collection of tags.
   Returns the next delimiter index after the current."
  [current char-types]
  (let [steps-from-current (->> (subvec char-types (inc current))
                                (take-while (complement delimiter?))
                                (count))
        next (+ 1 current steps-from-current)]
    (when (< next (count char-types))
      next)))

(defn previous-opener
  "Returns the previous opener before the closer index, of the same char."
  [closer closer-char children openers-bottom]
  (let [children-stripe (reverse (subvec children
                                         (get openers-bottom closer-char)
                                         closer))
        steps-back (->> children-stripe
                        (take-while #(or (not= closer-char (:tag %))
                                         (not (:opener? %))))
                        (count))
        previous (- closer
                    steps-back
                    (get openers-bottom closer-char)
                    1)]
    (when (not-neg? previous) previous)))

(defn delimiter->text
  "Converts a delimiter to a text node."
  [{:keys [tag delims]}]
  (new-node :text (string/join (repeat delims tag))))

(defn remove-delimiters
  "Converts candidate delimiters to normal text in case of no match.
   Multiple arity operates only between start and end - both not included."
  [children]
  (map #(if (delimiter? (:tag %))
          (delimiter->text %)
          %)
       children))

(defn update-delimiters
  "Subtracts the given delimiters to the node n. If new count is 0, retuns nil.
   WARNING: returns a list with one element or nil."
  [n delims]
  (let [new-ds (- (:delims n) delims)]
    (when (pos? new-ds)
      [(assoc n :delims new-ds)])))

(defn normalize-nodes
  "Finalize the nodes list after having processed emphasis"
  [nodes]
  (-> nodes remove-delimiters ast/compact-text-nodes))

(defn process-emph
  "Process the children of the current node to generate emphasis inlines.
  Stack bottom is the index of the child to wich we can go back."
  ([nodes]
   (process-emph nodes 0))
  ([nodes stack-bottom]
  ;; TODO: trasform to \u2019 if singlequote, \u201C if double
   (let [init-children (subvec nodes stack-bottom)
         init-children-types (mapv :tag init-children)]
     (loop [children nodes
            openers-bottom {c/Asterisk    stack-bottom
                            c/Underscore  stack-bottom
                            c/Singlequote stack-bottom
                            c/Doublequote stack-bottom}
            closer (as-> [c/Asterisk c/Underscore] cs
                         (mapv #(index-of-next % init-children-types) cs)
                         (remove nil? cs)
                         (when (not-empty cs)
                           (apply min cs)))]
       (let [children-types (mapv :tag children)]
         (if-not closer
           (normalize-nodes children)
           (let [closer-node (nth children closer)
                 char (:tag closer-node)
                 opener (previous-opener closer char children openers-bottom)
                 opener-node (when opener
                               (nth children opener))
                 opener-delims (:delims opener-node)
                 closer-delims (:delims closer-node)]
             (if (and (:closer? closer-node) opener-delims) ;; TODO: get rid of nil in opener-delims
               ;; Good, we found a matching opener-closer pair! Things to do:
               ;; 1. Figure out if it's emph or strong emph
               ;; 2. Put all the nodes between the opener and closer inside
               ;;    the new
               ;; 3. Convert the delims inside to text, compact the text nodes
               ;; 4. Insert the new node *after* the opener
               ;; 5. Subtract the # of delimiters from opener and closer
               ;; 6. If a delimiter becomes empty, remove it.
               (let [delims (min 2 opener-delims closer-delims)
                     strong? (> delims 1) ;; 1.
                     emph-node (new-node (if strong? :strong :emph)
                                         {}
                                         (-> children
                                             (subvec (inc opener) closer) ; 2.
                                             (remove-delimiters) ; 3a.
                                             (ast/compact-text-nodes))) ; 3b.
                     [new-op new-cl] (mapv #(update-delimiters % delims) ; 5, 6.
                                           [opener-node closer-node])
                     new-children (fold [(subvec children 0 opener)
                                         new-op
                                         [emph-node] ; 4.
                                         new-cl
                                         (subvec children (inc closer))])
                     right-number (- (count children)
                                     (count new-children))
                     new-children-types (mapv :tag new-children)]
                 (recur new-children
                        openers-bottom
                        (next-delimiter (- closer right-number)
                                        new-children-types)))
               ;; No opener around here. We proceed to:
               ;; 1. Advance the stack bottom for the current char to current
               ;; 2. Convert the current node to text if also not an opener
               ;; 3. Advance the current element
               (recur (if-not (:opener? closer-node) ; 2.
                        (update children
                                closer
                                delimiter->text)
                        children)
                      (assoc openers-bottom char (dec closer)) ; 1.
                      (next-delimiter closer children-types)))))))))) ; 3.