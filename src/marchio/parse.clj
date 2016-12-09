(ns marchio.parse
  (:require
    [clojure.string :as string]
    [clojure.walk :refer [postwalk]]
    [marchio.re :as re]
    [marchio.inlines :as inlines]
    [marchio.blocks :as blocks]
    [clojure.zip :as zip]))

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

;; Block types: Document, BlockQuote, ListItem, FencedCode,
;; IndentedCode, RawHtmlBlock, Reference

; 'finalize' is run when the block is closed.
; 'continue' is run to check whether the block is continuing
; at a certain line and offset (e.g. whether a block quote
; contains a `>`.  It returns 0 for matched, 1 for not matched,
; and 2 for "we've dealt with this line completely, go to next."}})
(defprotocol Block
  (match?        [this line])
  (finalize      [this])
  (can-contain   [this that])
  (accepts-lines [this]))

(def leaf-blocks
  [:thematic-break
   :atx-heading
   :setext-heading
   :indented-code-block
   :fenced-code-block
   :html-block
   :link-ref-definition
   :paragraph
   :blank-line])

(def container-blocks
  [:block-quote
   :list
   :list-item])

(def inline-blocks
  [:backslash-escape
   :entity-ref
   :code-span
   :emphasis
   :link
   :image
   :autolink
   :raw-html
   :hard-line-break
   :soft-line-breaks])

(defn- remove-insecure
  "Removes the UTF8 char \u0000"
  [in]
  (string/replace in "\u0000" "\uFFFD"))

(defn- split-lines
  "Split input in a sequence of lines"
  [in]
  (string/split in re/line-ending))

;; -- API

(defn text->ast
  "Parse an UTF8 input string to a CommonMark Abstract Syntax Tree"
  [input]
  (->> input
       (split-lines)
       (map remove-insecure)
       (blocks/parse)
       (inlines/parse)
       (zip/root)))

;; TODO
(defn ast->hiccup
  "Convert an Abstract Syntax Tree to Hiccup"
  [tree]
  tree)

(comment
  (def tests (marchio.test/load-spec-tests))
  (def sample (:markdown (nth tests 16)))
  (def test-cats (group-by :section tests))
  (keys test-cats)
  (marchio.test/get-cmark-ast
    (->> "Emphasis and strong emphasis"
         (get test-cats)
         (first)
         (:markdown)))
  (re-seq #"\p{L}+" "prøve"))