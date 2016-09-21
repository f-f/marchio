(ns marchio.inlines
  (:require
    [clojure.string :as string]
    [clojure.core.match :refer [match]]
    [marchio.chars :refer [Asterisk Newline Underscore Singlequote Doublequote]]
    [marchio.re :as re :refer [match-char]]))

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

(defn scan-delims
  "Search for delimiter runs (asterisk, underscore) or quotes, and return a
   vector with the rest of the string and a new Delimiter entity, with info
   about the length of the run, and if they can open or close sequences."
  [string prev-char]
  (let [chars    (seq string)
        before   (or (if (char? prev-char)
                       prev-char
                       (first (seq prev-char)))
                     Newline)
        char     (first chars)
        after    (or (second chars) Newline)
        run      (take-while (partial = char) chars)
        run-comp (drop-while (partial = char) chars)
        [delims new-chars] (if (or (= char Singlequote)
                                   (= char Doublequote))
                             [1           (rest chars)]
                             [(count run) run-comp])
        [before-white? after-white?] (mapv #(match-char re/unicode-whitespace %)
                                           [before after])
        [before-punct? after-punct?] (mapv #(match-char re/punctuation %)
                                           [before after])
        left-flanking (and (not after-white?)
                           (not (and after-punct?
                                     (not before-white?)
                                     (not before-punct?))))
        right-flanking (and (not before-white?)
                            (not (and before-punct?
                                      (not after-white?)
                                      (not after-punct?))))
        [opener? closer?]
        (condp = char
          Underscore       [(and left-flanking
                                 (or (not right-flanking)
                                     before-punct?))
                            (and right-flanking
                                 (or (not left-flanking)
                                     after-punct?))]
          (or Singlequote
              Doublequote) [(and left-flanking (not right-flanking))
                            right-flanking]
                           [left-flanking right-flanking])]
    (when-not (and (empty? chars) (zero? delims))
      [new-chars
       {:type char
        :delims delims
        :opener? opener?
        :closer? closer?}])))

(defn handle-delim
  "Handle a delimiter marker for emphasis or a quote."
  [char block])

(defn parse-next-char
  "Take a sequence of chars, return a new sequence of the rest of the chars and
   a new node."
  [chars prev-char]
  (let [c (first chars)]
    (condp = c
      Asterisk   (scan-delims chars prev-char) ;; TODO: introduce handle-delims
      Underscore (scan-delims chars prev-char)
      [(rest chars)
       {:type :text :attrs {:val (str c)}}]))) ;; TODO: introduce text records

;; TODO
(defn process-emph
  "Process the children of the current node to generate emphasis inlines.
  Stack bottom is the index of the child to wich we can go back."
  [node stack-bottom]
  node)
  ;(let [children (subvec (:children node) stack-bottom)]
  ;  (loop [current-position (min-pos (.indexOf (map :type children) Underscore)
  ;                                   (.indexOf (map :type children) Asterisk)])

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
            _ (println (count chars) read-chars)
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
  (parse-text sample n)
  (def k #(-> %
              (string/replace "var" "(def")
              (string/replace #"C_\S+" (fn [s] (-> s (subs 2)
                                                   string/capitalize)))
              (string/replace "= " "(char ")
              (string/replace ";" "))"))))