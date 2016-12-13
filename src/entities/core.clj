(ns entities.core
  (:require [clojure.string :as string]
            [djy.char :as c]
            [cheshire.core :refer [parse-string]]))

; Inspiration, help, magic
; https://mathiasbynens.be/notes/javascript-unicode
; https://github.com/fb55/entities

;; TODO: generate a namespace out of this
(defonce entities-map
  (-> (slurp "https://html.spec.whatwg.org/entities.json")
      (parse-string)))

(def decode-map
  "Map of codepoint conversions"
  {0   65533
   128 8364
   130 8218
   131 402
   132 8222
   133 8230
   134 8224
   135 8225
   136 710
   137 8240
   138 352
   139 8249
   140 338
   142 381
   145 8216
   146 8217
   147 8220
   148 8221
   149 8226
   150 8211
   151 8212
   152 732
   153 8482
   154 353
   155 8250
   156 339
   158 382
   159 376})

(defn decode-code-point [cp]
  (if (or (> cp 0x10FFFF)
          (c/surrogate? cp))
    "\uFFFD"
    (str (c/char' (if (contains? decode-map cp)
                    (get decode-map cp)
                    cp)))))

(defn escape []
  "TODO")

(defn encode-xml []
  "TODO")

(defn encode-html []
  "TODO")

(defn decode-xml []
  "TODO")

(defn decode-html [^String in]
  (string/replace
    in
    (re-pattern (str "(?i)^&(?:#[xX][\\da-fA-F]+|#\\d+|"
                     (string/join "|" (map #(subs % 1 (dec (count %)))
                                           (keys entities-map)))
                     ");"))
    (fn [m]
      (if (= (nth m 1) \#)
        (if (contains? #{\X \x} (nth m 2))
          (-> (subs m 3 (dec (count m)))
              (Integer/parseInt 16)
              (decode-code-point))
          (-> (subs m 2 (dec (count m)))
              (Integer/parseInt 10)
              (decode-code-point)))
        (get-in entities-map [m "characters"])))))

(comment
  (map #(format "%04x" (int %)) (seq (c/char' 0x1F4A9))))