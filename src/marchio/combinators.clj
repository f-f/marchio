(ns marchio.combinators
  (:require [blancas.kern.core :as k]))

(def any
  "Parser that accepts any character as input, returns string."
  (k/<+> (k/many k/any-char)))