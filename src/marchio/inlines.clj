(ns marchio.inlines
  (:require
    [clojure.string :as string]
    [uncomplicate.fluokitten.core :refer [fmap fapply bind return fold foldmap]]
    [uncomplicate.fluokitten.jvm]
    [marchio.chars :refer [Asterisk Newline Underscore Singlequote Doublequote]]
    [marchio.re :as re :refer [match]]
    [marchio.ast :refer [new-node]]))

;; -- Phase 2
;; -- Inline parsers

(def not-neg? (complement neg?))

(defn append-char [node char]
  (update node :content
          (fn [children]
            (if-not (string? (last children))
              (conj (or children []) char)
              (assoc children (dec (count children))
                              (str (last children) char))))))

(defn append-children [node new]
  (update node :content conj new))

(defn parse-emphasis
  "Search for delimiter runs (asterisk, underscore) or quotes, and return a
   vector with the rest of the string and a new Delimiter entity, with info
   about the length of the run, and if they can open or close sequences."
  [string prev-char]
  (let [chars  (seq string)
        before (or (if (char? prev-char)
                     prev-char
                     (first (seq prev-char)))
                   Newline)
        char   (first chars)
        after  (or (second chars) Newline)
        [run run-comp] (mapv #(% (partial = char) chars)
                             [take-while drop-while])
        [delims new-chars] (if (or (= char Singlequote)
                                   (= char Doublequote))
                             [1           (rest chars)]
                             [(count run) run-comp])
        [before-white? after-white?] (mapv #(match re/unicode-whitespace %)
                                           [before after])
        [before-punct? after-punct?] (mapv #(match re/punctuation %)
                                           [before after])
        left-flanking (and (not after-white?)
                           (not (and after-punct?
                                     (not before-white?)
                                     (not before-punct?))))
        right-flanking (and (not before-white?)
                            (not (and before-punct?
                                      (not after-white?)
                                      (not after-punct?))))
        underscore-flags [(and left-flanking
                               (or (not right-flanking)
                                   before-punct?))
                          (and right-flanking
                               (or (not left-flanking)
                                   after-punct?))]
        quotes-flags [(and left-flanking
                           (not right-flanking))
                      right-flanking]
        [opener? closer?] (condp = char
                            Underscore  underscore-flags
                            Singlequote quotes-flags
                            Doublequote quotes-flags
                            [left-flanking right-flanking])]
    (when-not (and (empty? chars) (zero? delims))
      [new-chars
       {:tag char
        :delims delims
        :opener? opener?
        :closer? closer?}])))

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
  (some #{char} [Underscore Asterisk Singlequote Doublequote]))

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

(defn compact-text-nodes
  "Given a list of text nodes, join together the contiguous ones."
  [children]
  (reduce (fn [new-vec el]
            (if (and (= (:tag el) :text)
                     (= (:tag (last new-vec)) :text))
              (conj (pop new-vec)
                    (update-in (last new-vec)
                               [:content 0]
                               #(str % (first (:content el)))))
              (conj new-vec el)))
          []
          children))

(defn update-delimiters
  "Subtracts the given delimiters to the node n. If new count is 0, retuns nil.
   WARNING: returns a list with one element or nil."
  [n delims]
  (let [new-ds (- (:delims n) delims)]
    (when (pos? new-ds)
      [(assoc n :delims new-ds)])))

(defn normalize-node
  "Finalize the node after having processed emphasis"
  [node]
  (update node :content #(-> % remove-delimiters compact-text-nodes)))

(defn process-emph
  "Process the children of the current node to generate emphasis inlines.
  Stack bottom is the index of the child to wich we can go back."
  [node stack-bottom]
  ;; TODO: trasform to \u2019 if singlequote, \u201C if double
  (let [init-children (subvec (:content node) stack-bottom)
        init-children-types (mapv :tag init-children)]
    (loop [current-node node
           openers-bottom {Asterisk    stack-bottom
                           Underscore  stack-bottom
                           Singlequote stack-bottom
                           Doublequote stack-bottom}
           closer (as-> [Asterisk Underscore] cs
                        (mapv #(index-of-next % init-children-types) cs)
                        (remove nil? cs)
                        (when (not-empty cs)
                          (apply min cs)))]
      (let [children (:content current-node)
            children-types (mapv :tag children)]
        (if-not closer
          (normalize-node current-node)
          (let [closer-node (nth children closer)
                char (:tag closer-node)
                opener (previous-opener closer char children openers-bottom)
                opener-node (when opener
                              (nth children opener))
                opener-delims (:delims opener-node)
                closer-delims (:delims closer-node)]
            (if (and (:closer? closer-node) opener)
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
                                            (compact-text-nodes))) ; 3b.
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
                (recur (assoc current-node :content new-children)
                       openers-bottom
                       (next-delimiter (- closer right-number)
                                       new-children-types)))
              ;; No opener around here. We proceed to:
              ;; 1. Advance the stack bottom for the current char to current
              ;; 2. Convert the current node to text if also not an opener
              ;; 3. Advance the current element
              (recur (if-not (:opener? closer-node) ; 2.
                       (update-in current-node
                                  [:content closer]
                                  delimiter->text)
                       current-node)
                     (assoc openers-bottom char (dec closer)) ; 1.
                     (next-delimiter closer children-types))))))))) ; 3.

(defn parse-next-char
  "Take a sequence of chars, return a new sequence of the rest of the chars and
   a new node."
  [chars prev-char]
  (let [c (first chars)]
    (condp = c
      Asterisk   (parse-emphasis chars prev-char)
      Underscore (parse-emphasis chars prev-char)
      [(rest chars)
       (new-node :text (str c))])))

(defn parse-text [line block]
  (loop [chars (seq line)
         pos   0
         prev  nil
         node  block]
    (if (empty? chars)
      (process-emph node 0)
      (let [[new-chars new-node] (parse-next-char chars prev)
            read-chars (- (count chars) (count new-chars))
            new-pos (+ pos read-chars)
            new-prev (nth chars (dec read-chars))]
        (recur new-chars
               new-pos
               new-prev
               (append-children node new-node))))))

;; TODO
;; Remember to parse inlines only inside paragraphs or headings
(defn parse
  "AST building, Phase 2: walk the half-open tree and parse inline text."
  [tree]
  tree)

(comment
  (def sample "*foo bar*\n")
  (def sample2 "**foo bar** \n")
  (def n (marchio.ast/new-node :paragraph))
  (append-char (append-char n sample) "aaa")
  (append-char (append-children (append-char n sample) n) "aaa")
  (parse-next-char sample nil)
  (marchio.test/get-cmark-ast sample)
  (marchio.test/get-cmark-ast sample2)
  (parse-text sample2 n))