(ns marchio.combinators
  (:require [blancas.kern.core :as k]))

;; TODO: spec it
(defmacro defparser
  "Boilerplate for a backtracking parser"
  [name bindings return-body]
  `(def ~name
     (k/<:> (k/bind ~bindings (k/return ~return-body)))))

(def any
  "Parser that accepts any character as input, returns string."
  (k/<+> (k/many k/any-char)))