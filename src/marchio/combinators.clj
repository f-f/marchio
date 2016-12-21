(ns marchio.combinators
  (:require
    [blancas.kern.core :as k]
    [marchio.re :as re]
    [uncomplicate.fluokitten.core :refer [fmap fapply bind return fold foldmap op]]
    [uncomplicate.fluokitten.jvm]
    [clojure.string :as string]))

;; TODO: this ns is a big pile of random stuff, please clean it up. Please.

(defn consumed-by [p]
  (k/<:> (k/bind [i-bef k/get-input
                  res   p
                  i-aft k/get-input]
                 (k/return [res
                            (apply str (take (- (count i-bef)
                                                (count i-aft))
                                             i-bef))]))))


;; HACK WARNING HACK WARNING HACK WARNING
;;
;; Here in this next macro `consumed-by` is employed to save the value
;; of the last char parsed in the user state of the parser. While this is
;; totally possible, it doesn't look good nor recommendable.
;; This low level thing - aka keeping the last char parsed - should be kept
;; in the internal state of the parser; this would require forking kern though.
;;
;; HACK WARNING HACK WARNING HACK WARNING
(defmacro defparser ;; TODO: spec it
  "Boilerplate for a backtracking parser"
  ([name bindings return-body]
   `(def ~name
      (k/bind
        [res+count# (consumed-by
                      (k/<:> (k/bind
                               ~bindings
                               (k/return ~return-body))))
         _# (k/modify-state #(assoc % :last (last (second res+count#))))]
        (k/return (first res+count#)))))
  ([name bindings test return-body]
   `(def ~name
      (k/bind
        [res+count# (consumed-by
                      (k/<:> (k/bind
                               ~bindings
                               (if ~test
                                 (k/return ~return-body)
                                 (k/fail "Failed.")))))
         _# (k/modify-state #(assoc % :last (last (second res+count#))))]
        (k/return (first res+count#))))))


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

(defn word-from-re [r]
  (k/<:> (k/bind [i k/get-input
                  p k/get-position]
           (if-let [m (re/match r (string/join i))]
             (k/bind [_ (k/set-input (drop (count m) i))
                      _ (k/set-position (str-pos p (take (count m) i)))
                      _ k/clear-empty]
               (k/return m))
             (k/fail "No match from regex")))))

(def any
  "Parser that accepts any character as input, returns string."
  (k/<+> (k/many k/any-char)))
