(ns marchio.ast
  (:require
    [clojure.string :as string]
    [clojure.walk :refer [postwalk]]))

(def re-thematic-break #"^(?:(?:\*[ \t]*){3,}|(?:_[ \t]*){3,}|(?:-[ \t]*){3,})[ \t]*$")
(def re-maybe-special #"^[#`~*+_=<>0-9-]")
(def re-non-space #"[^ \t\f\v\r\n\u000B]")
(def re-bullet-list-marker #"^[*+-]")
(def re-ordered-list-marker #"^(\d{1,9})([.])")
(def re-ATX-heading-marker #"^#{1,6}(?:[ \t]+|$)")
(def re-code-fence #"^`{3,}(?!.*`)|^~{3,}(?!.*~)")
(def re-closing-code-fence #"^(?:`{3,}|~{3,})(?= *$)")
(def re-setext-heading-line #"^(?:=+|-+)[ \t]*$")
(def re-line-ending #"\r\n|\n|\r")

(defn is-blank?
  "Returns true if string contains only space characters."
  [s]
  (not (re-find re-non-space s)))

;; --  Parsing strategy

;; Parsing has two phases:

;; 1. Input is split by lines, that are then consumed. While doing this the
;;    block structure of the document is constructed: paragraphs, list items,
;;    block quotes, etc. Text is assigned to those blocks but not parsed.
;;    Also, a map of internal links is built from link reference definitions.

;; 2. Raw contents of paragraphs and headings are parsed into seqs of Markdown
;;    inline elements (spans, links, strings, etc), using the map of link refs.
;;    The documents is always represented as a Tree of blocks. The root of the
;;    Tree is a Document block, which may have a number of children, which could
;;    then have children, etc.
;;    Blocks could be 'open' or 'closed'. Last child of a block is considered
;;    'open', meaning that the next lines of input can alter its content.


;; -- Phase 1: Block Structure

;; Each line that is processed has an effect on the tree. Every line may alter
;; the document in atleast one of the following ways:

;; 1. One or more open blocks may be closed.
;; 2. One or more new blocks may be created as children of the last open block.
;; 3. Text may be added to the last (deepest) open block remaining on the tree.

;; For each line, this procedure is executed:

;; 1. Given that the line must satisfy a certain condition to keep a block open
;;    (e.g. a block quote requires a > character, a paragraph requires a
;;    non-blank line), iterate through the open blocks - starting from the root
;;    and down to the last open block through last-children-path - and try to
;;    match the condition of each block.
;;    But can't close unmatched blocks, there may be a lazy continuation line.
;; 2. Look for new block starts (e.g. > for a block quote).
;;    On encountering a new block start, close any blocks unmatched in step 1
;;    before creating the new block as a child of the last matched block.
;; 3. Look at the remainder of the line: thi is text that can be incorporated
;;    into the last open block (paragraph, code block, heading, or raw HTML).

(def Document
  {:children []
   :level    0
   :open?    true
   :link-map {}})

(defn parse-line [tree line]
  (update tree :children conj line))

(defn close-blocks
  ""
  [tree]
  (->> tree
       (postwalk
         (fn [el]
           (if (and (map? el)
                    (contains? el :open?))
             (assoc el :open? false)
             el)))))

(defn block-structure
  "AST Building, Phase 1: parse lines of text, and recursively build an AST
   with only block structure."
  [lines]
  (loop [tree Document
         ls   lines]
    (println tree)
    (println ls (count ls))
    (if (empty? ls)
      (close-blocks tree) ;; I guess it's not "necessary" to close blocks
      (recur (parse-line tree (first ls))
             (rest ls)))))

;; -- Phase 2

;; TODO
(defn inline-structure
  "AST building, Phase 2: walk the half-open tree and parse inline text."
  [tree]
  tree)

;; -- API

(defn- remove-insecure
  "Removes the UTF8 char \u0000"
  [in]
  (string/replace in "\u0000" "\uFFFD"))

(defn- split-lines
  "Split input in a sequence of lines"
  [in]
  (string/split in re-line-ending))

(defn parse
  "Convert an UTF8 input string to a CommonMark Abstract Syntax Tree"
  [input]
  (-> input
      (remove-insecure)
      (split-lines)
      (block-structure)
      (inline-structure)))

;; TODO
(defn ast->hiccup
  "Convert an Abstract Syntax Tree to Hiccup"
  [tree]
  tree)

(comment
  (def tests (marchio.spec-tests/load-tests))
  (def sample (:markdown (nth tests 16)))
  (re-seq #"\p{L}+" "prøve"))