(ns marchio.inlines
  (:require
    [clojure.string :as string]
    [blancas.kern.core :as k]
    [marchio.chars :as c]
    [marchio.re :as re :refer [match]]
    [marchio.ast :as ast :refer [new-node]]
    [marchio.combinators :as cs :refer [defparser]]
    [marchio.inlines.emph :as emph]
    [clojure.string :as string]
    [entities.core :refer [decode-html]]))

;; -- Phase 2
;; -- Inline Parsers -----------------------------------------------------------

(defparser TextChar
  [t k/any-char]
  (re/match re/not-inline-special t)
  t)

;; if we encounter a nonrelevant char, just eat as many chars as possible
(defparser Text
  [t (k/<+> (k/many1 TextChar))]
  (new-node :text t))

(defparser Hardbreak
  [_ (k/<|> (k/<:> (k/<*> (k/times 2 (k/sym* c/Space))
                          (k/skip-many (k/sym* c/Space))))
            (k/sym* c/Backslash))
   _ (cs/from-re re/line-ending)
   _ (k/skip-many (k/sym* c/Space))]
  (new-node :linebreak))

(defparser Softbreak
  [_ (cs/from-re re/line-ending)
   _ (k/skip-many (k/sym* c/Space))]
  (new-node :softbreak))

;; Backslash, 1
(defparser EscapedChar
  [_ (k/sym* c/Backslash)
   t (cs/from-re re/escapable)]
  (new-node :text (str t)))

;; Backslash, 2
(defparser Backslash
  [_ (k/sym* c/Backslash)]
  (new-node :text "\\"))

;; Backticks 1: matching codeblock
(defparser InlineCode
  [open  (k/many1 (k/sym* c/Backtick))
   text  (k/<+>
           (k/many1
             (k/<|> (k/<:> (k/bind
                             [w (k/<+> (k/many1 (k/sym* c/Backtick)))]
                             (if (= (count w) (count open))
                               (k/fail "Backtick unmatched")
                               (k/return w))))
                    (k/<$> (fn [_] c/Space)
                           (k/many1 (cs/from-re re/space)))
                    (k/many1 (k/none-of* (str c/Backtick re/spaces-string))))))
   close (k/<*>
           (if (< (count open) 2)
             (k/sym* c/Backtick)
             (k/times (count open) (k/sym* c/Backtick)))
           (k/not-followed-by (k/sym* c/Backtick)))]
  (new-node :code (string/trim (apply str text))))

;; Backticks 2: simple literal
(defparser Backticks
  [ticks (k/many1 (k/sym* c/Backtick))]
  (new-node :text (apply str ticks)))

;; Delimiters runs (asterisk, underscore) or quotes.
;; Info returned: run length, and if they can open and/or close sequences.
(defparser Emph
  [s     k/get-state
   c     (k/<|> (k/sym* c/Singlequote)
                (k/sym* c/Doublequote)
                (k/many1 (k/sym* c/Asterisk))
                (k/many1 (k/sym* c/Underscore)))
   after (k/look-ahead k/any-char)]
  (let [[num char] (if (vector? c)
                     [(count c) (first c)]
                     [1         c])
        before (:last s)
        after (if after after \newline)
        bef-white? (match re/unicode-whitespace before)
        bef-punct? (match re/punctuation before)
        aft-white? (match re/unicode-whitespace after)
        aft-punct? (match re/punctuation after)
        left-flanking (and (not aft-white?)
                           (not (and aft-punct?
                                     (not bef-white?)
                                     (not bef-punct?))))
        right-flanking (and (not bef-white?)
                            (not (and bef-punct?
                                      (not aft-white?)
                                      (not aft-punct?))))
        underscore-flags [(and left-flanking
                               (or (not right-flanking)
                                   bef-punct?))
                          (and right-flanking
                               (or (not left-flanking)
                                   aft-punct?))]
        quotes-flags [(and left-flanking
                           (not right-flanking))
                      right-flanking]
        [opener? closer?] (condp = char
                            c/Underscore  underscore-flags
                            c/Singlequote quotes-flags
                            c/Doublequote quotes-flags
                            [left-flanking right-flanking])]
    {:tag char
     :delims num
     :opener? opener?
     :closer? closer?}))

(defparser LinkOrImage
  [_     (k/token* "![" "[")
   title (k/many1 k/any-char)
   _     (k/sym* "]")]
  (new-node :link title))

(defparser LinkOpener
  [_ (k/sym* c/OpenBracket)]
  (new-node :text "["))
  ;{:image? false
  ; :active? true
  ; :label ""
  ; :content []])

(defparser ImageOpener
  [_ (k/sym* c/Bang)
   _ (k/sym* c/OpenBracket)]
  {:image? true
   :active? true
   :label ""
   :content []})

(defparser CloseBracket
  [_ (k/sym* c/CloseBracket)]
  (new-node :text "]"))

(defparser InlineHTML
  [_ (k/look-ahead (k/sym* c/LessThan))
   n (k/<|>
       (k/<$> (fn [r] (new-node :html_inline r)) (cs/word-from-re re/html-tag))
       (k/<$> (fn [_] (new-node :text "<"))      (k/sym* c/LessThan)))]
  n)

(defparser Entity
  [e (cs/word-from-re re/entity)]
  (new-node :text (decode-html e)))

;; Fallback, just text
(defparser Fallback
  [t k/any-char]
  (new-node :text (str t)))

(def Inlines
  (k/<|> Text
         Hardbreak
         Softbreak
         EscapedChar
         Backslash
         InlineCode
         Backticks
         Emph
         LinkOpener
         ImageOpener
         CloseBracket
         InlineHTML
         Entity
         Fallback))

;; -- API ----------------------------------------------------------------------

(defn parse-line
  "Parses one line searching for inlines, which then converts to nodes."
  [children]
  ;(println (str children))
  (let [line (first children)]
    (-> (k/many1 Inlines)
        (k/value line "Inlines" {:last \newline})
        (emph/process-emph))))

(defn parse
  "AST building, Phase 2: walk the half-open tree and parse inline text;
   build inlines nodes only out of text inside paragraphs or headings."
  [tree]
  (ast/update-tree
    tree
    #(contains? #{:paragraph :heading} (:tag %))
    #(update % :content parse-line)))

(comment
  (def sample "*foo bar*\n")
  (def sample2 "**foo bar** \n")
  (def n (marchio.ast/new-node :paragraph))
  (append-char (append-char n sample) "aaa")
  (append-char (append-children (append-char n sample) n) "aaa")
  (marchio.test/get-cmark-ast "aaa\nbbb  \nfoo")
  (marchio.test/get-cmark-ast sample2)
  (marchio.test/get-cmark-ast "\\[")
  (marchio.test/get-cmark-ast "``aa`b`")
  (-> (ast/new-tree)
      (ast/append-child (new-node :paragraph "aaa\nbbb  \nfoo"))
      (ast/append-child (new-node :paragraph "\\["))
      (ast/append-child (new-node :paragraph "``aa`b`"))
      (ast/append-child (new-node :paragraph sample2))
      (parse)))
