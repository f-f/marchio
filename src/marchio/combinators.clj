(ns marchio.combinators
  (:require [blancas.kern.core :as k]
            [marchio.re :as re]
            [clojure.string :as string]))

;; TODO: spec it
(defmacro defparser
  "Boilerplate for a backtracking parser"
  ([name bindings return-body]
   `(def ~name
      (k/<:> (k/bind
               ~bindings
               (k/return ~return-body)))))
  ([name bindings test return-body]
   `(def ~name
      (k/<:> (k/bind
               ~bindings
               (if ~test
                 (k/return ~return-body)
                 (k/fail "Failed.")))))))

(defn char-pos
  "Computes the new position of the character c."
  [pos c]
  (cond (= c \newline) (assoc pos :col 1 :line (inc (:line pos)))
        (= c \tab)     (assoc pos :col (+ (:col pos) k/*tab-width*))
        :else          (assoc pos :col (inc (:col pos)))))

(defn str-pos
  "Computes the stream position after the character sequence cs."
  [pos cs]
  (if (empty? cs)
    pos
    (recur (char-pos pos (first cs))
           (rest cs))))

(defn from-re [re]
  (k/<:> (k/bind [c k/any-char]
           (if (re/match re c)
             (k/return (str c))
             (k/fail "No match from regex")))))

(defn word-from-re [re]
  (k/<:> (k/bind [i k/get-input
                  p k/get-position]
           (if-let [m (re/match re (string/join i))]
             (k/bind [_ (k/set-input (drop (count m) i))
                      _ (k/set-position (str-pos p (take (count m) i)))
                      _ k/clear-empty]
               (k/return m))
             (k/fail "No match from regex")))))

(def any
  "Parser that accepts any character as input, returns string."
  (k/<+> (k/many k/any-char)))

(defn consumed-by [p]
  (k/<:> (k/bind [i-bef k/get-input
                  _     p
                  i-aft k/get-input]
           (k/return (apply str (take (- (count i-bef)
                                         (count i-aft))
                                      i-bef))))))