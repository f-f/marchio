(ns marchio.combinators
  (:require [blancas.kern.core :as k]
            [marchio.re :as re]))

;; TODO: spec it
(defmacro defparser
  "Boilerplate for a backtracking parser"
  [name bindings return-body]
  `(def ~name
     (k/<:> (k/bind
              ~bindings
              (k/return ~return-body)))))

(defn from-re [re]
  (k/<:> (k/bind [c k/any-char]
           (if (re/match re c)
             (k/return (str c))
             (k/fail "No match from regex")))))

(def any
  "Parser that accepts any character as input, returns string."
  (k/<+> (k/many k/any-char)))