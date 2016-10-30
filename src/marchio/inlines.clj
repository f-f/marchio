(ns marchio.inlines
  (:require
    [clojure.string :as string]
    [marchio.chars :refer [Asterisk Newline Underscore Singlequote Doublequote]]
    [marchio.re :as re :refer [match]]))

;; -- Phase 2

;; -- Inline parsers

(defn append-char [node char]
  (update node :children
          (fn [children]
            (if-not (string? (last children))
              (conj (or children []) char)
              (assoc children (dec (count children))
                              (str (last children) char))))))

(defn append-children [node new]
  (update node :children conj new))

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
       {:type char
        :delims delims
        :opener? opener?
        :closer? closer?}])))

(defn index-of
  "Returns the index of first el in coll if found, otherwise nil"
  [coll el]
  (let [not-neg? (complement neg?)
        index    (.indexOf coll el)]
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
  "Returns the next delimiter index after the current"
  [current char-types]
  (let [steps-from-current (->> (subvec char-types (inc current))
                                (take-while (complement delimiter?))
                                (count))
        next (+ 1 current steps-from-current)]
    (when (< next (count char-types))
      next)))

(defn normalize-text
  "Converts candidate delimiters to normal text in case of no match"
  [node]
  (update node :children
    #(for [c %]
      (let [char (:type c)]
        (if (delimiter? char)
          {:type :text
           :attrs {}
           :children [(string/join (repeat (:delims c) char))]}
          c)))))

(defn previous-opener
  "Returns the previous opener before the closer index, of the same char."
  [closer closer-char children openers-bottom]
  (let [children-stripe (reverse (subvec children
                                         (get openers-bottom closer-char)
                                         closer))
        steps-back (->> children-stripe
                        (take-while #(or (not= closer-char (:type %))
                                         (not (:opener? %))))
                        (count))
        previous (- closer steps-back 1)]
    previous))

(defn process-emph
  "Process the children of the current node to generate emphasis inlines.
  Stack bottom is the index of the child to wich we can go back."
  [node stack-bottom]
  ;; TODO: trasform to \u2019 if singlequote, \u201C if double
  (let [children (subvec (:children node) stack-bottom)
        children-types (mapv :type children)]
    (loop [current-node node
           openers-bottom {Asterisk    stack-bottom
                           Underscore  stack-bottom
                           Singlequote stack-bottom
                           Doublequote stack-bottom}
           closer (->> [Asterisk Underscore]
                       (mapv #(index-of-next % children-types))
                       (remove nil?)
                       (#(when (not-empty %)
                          (apply min %))))]
      (if closer
        (let [closer-node (nth children closer)
              char (:type closer-node)
              opener (previous-opener closer char children openers-bottom)
              opener-node (nth children opener)
              odd-match? (and (or (:opener? closer-node)
                                  (:closer? opener-node))
                              (zero? (mod (+ (:delims opener-node)
                                             (:delims closer-node))
                                          3)))]
          (if (and (:closer? closer-node) opener)
            opener-node
            (recur current-node
                   openers-bottom
                   (next-delimiter closer children-types))))
        (normalize-text current-node)))))

(defn parse-next-char
  "Take a sequence of chars, return a new sequence of the rest of the chars and
   a new node."
  [chars prev-char]
  (let [c (first chars)]
    (condp = c
      Asterisk   (parse-emphasis chars prev-char)
      Underscore (parse-emphasis chars prev-char)
      [(rest chars)
       {:type :text :attrs {} :children [(str c)]}]))) ;; TODO: introduce text records

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
  (def n (marchio.test-utils/->Node :paragraph {} []))
  (append-char (append-char n sample) "aaa")
  (append-char (append-children (append-char n sample) n) "aaa")
  (parse-next-char sample nil)
  (parse-text "**foo bar** \n" n)
  (def k #(-> %
              (string/replace "var" "(def")
              (string/replace #"C_\S+" (fn [s] (-> s (subs 2)
                                                   string/capitalize)))
              (string/replace "= " "(char ")
              (string/replace ";" "))"))))