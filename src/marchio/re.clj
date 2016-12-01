(ns marchio.re
  (:require [marchio.chars :as c]))

;; -- Defs

(def code-indent 4)

;; -- Easy regexps

(def unicode-whitespace #"^\s")
(def punctuation #"[!-#%-\*,-/:;\?@\[-\]_\{\}\xA1\xA7\xAB\xB6\xB7\xBB\xBF\u037E\u0387\u055A-\u055F\u0589\u058A\u05BE\u05C0\u05C3\u05C6\u05F3\u05F4\u0609\u060A\u060C\u060D\u061B\u061E\u061F\u066A-\u066D\u06D4\u0700-\u070D\u07F7-\u07F9\u0830-\u083E\u085E\u0964\u0965\u0970\u0AF0\u0DF4\u0E4F\u0E5A\u0E5B\u0F04-\u0F12\u0F14\u0F3A-\u0F3D\u0F85\u0FD0-\u0FD4\u0FD9\u0FDA\u104A-\u104F\u10FB\u1360-\u1368\u1400\u166D\u166E\u169B\u169C\u16EB-\u16ED\u1735\u1736\u17D4-\u17D6\u17D8-\u17DA\u1800-\u180A\u1944\u1945\u1A1E\u1A1F\u1AA0-\u1AA6\u1AA8-\u1AAD\u1B5A-\u1B60\u1BFC-\u1BFF\u1C3B-\u1C3F\u1C7E\u1C7F\u1CC0-\u1CC7\u1CD3\u2010-\u2027\u2030-\u2043\u2045-\u2051\u2053-\u205E\u207D\u207E\u208D\u208E\u2308-\u230B\u2329\u232A\u2768-\u2775\u27C5\u27C6\u27E6-\u27EF\u2983-\u2998\u29D8-\u29DB\u29FC\u29FD\u2CF9-\u2CFC\u2CFE\u2CFF\u2D70\u2E00-\u2E2E\u2E30-\u2E42\u3001-\u3003\u3008-\u3011\u3014-\u301F\u3030\u303D\u30A0\u30FB\uA4FE\uA4FF\uA60D-\uA60F\uA673\uA67E\uA6F2-\uA6F7\uA874-\uA877\uA8CE\uA8CF\uA8F8-\uA8FA\uA8FC\uA92E\uA92F\uA95F\uA9C1-\uA9CD\uA9DE\uA9DF\uAA5C-\uAA5F\uAADE\uAADF\uAAF0\uAAF1\uABEB\uFD3E\uFD3F\uFE10-\uFE19\uFE30-\uFE52\uFE54-\uFE61\uFE63\uFE68\uFE6A\uFE6B\uFF01-\uFF03\uFF05-\uFF0A\uFF0C-\uFF0F\uFF1A\uFF1B\uFF1F\uFF20\uFF3B-\uFF3D\uFF3F\uFF5B\uFF5D\uFF5F-\uFF65]|\uD800[\uDD00-\uDD02\uDF9F\uDFD0]|\uD801\uDD6F|\uD802[\uDC57\uDD1F\uDD3F\uDE50-\uDE58\uDE7F\uDEF0-\uDEF6\uDF39-\uDF3F\uDF99-\uDF9C]|\uD804[\uDC47-\uDC4D\uDCBB\uDCBC\uDCBE-\uDCC1\uDD40-\uDD43\uDD74\uDD75\uDDC5-\uDDC9\uDDCD\uDDDB\uDDDD-\uDDDF\uDE38-\uDE3D\uDEA9]|\uD805[\uDCC6\uDDC1-\uDDD7\uDE41-\uDE43\uDF3C-\uDF3E]|\uD809[\uDC70-\uDC74]|\uD81A[\uDE6E\uDE6F\uDEF5\uDF37-\uDF3B\uDF44]|\uD82F\uDC9F|\uD836[\uDE87-\uDE8B]")
(def thematic-break #"^(?:(?:\*[ \t]*){3,}|(?:_[ \t]*){3,}|(?:-[ \t]*){3,})[ \t]*$")
(def maybe-special #"^[#`~*+_=<>0-9-]")
(def non-space #"[^ \t\f\v\r\n\u000B]")
(def bullet-list-marker #"^[*+-]")
(def ordered-list-marker #"^(\d{1,9})([.])")
(def ATX-heading-marker #"^#{1,6}(?:[ \t]+|$)")
(def code-fence #"^`{3,}(?!.*`)|^~{3,}(?!.*~)")
(def closing-code-fence #"^(?:`{3,}|~{3,})(?= *$)")
(def setext-heading-line #"^(?:=+|-+)[ \t]*$")
(def line-ending #"\r\n|\n|\r")
(def escapable #"^[!\"#$%&'()*+,./:;<=>?@\\\[\\\]^_`{|}~-]")

;; -- Html regexps

(def TAGNAME "[A-Za-z][A-Za-z0-9-]*")
(def ATTRIBUTENAME "[a-zA-Z_:][a-zA-Z0-9:._-]*")
(def UNQUOTEDVALUE "[^\"'=<>`\\x00-\\x20]+")
(def SINGLEQUOTEDVALUE "'[^']*'")
(def DOUBLEQUOTEDVALUE "\"[^\"]*\"")
(def ATTRIBUTEVALUE (str "(?:" UNQUOTEDVALUE "|" SINGLEQUOTEDVALUE "|" DOUBLEQUOTEDVALUE ")"))
(def ATTRIBUTEVALUESPEC (str "(?:" "\\s*=" "\\s*" ATTRIBUTEVALUE ")"))
(def ATTRIBUTE (str "(?:" "\\s+" ATTRIBUTENAME ATTRIBUTEVALUESPEC "?)"))
(def OPENTAG (str "<" TAGNAME ATTRIBUTE "*" "\\s*/?>"))
(def CLOSETAG (str "</" TAGNAME "\\s*[>]"))

(def html-block-open
  [#"."
   #"(?i)^<(?:script|pre|style)(?:\s|>|$)" ;; (?i) is for case insensitive!
   #"^<!--"
   #"^<[?]"
   #"^<![A-Z]"
   #"^<!\[CDATA\["
   #"^<[/]?(?:address|article|aside|base|basefont|blockquote|body|caption|center|col|colgroup|dd|details|dialog|dir|div|dl|dt|fieldset|figcaption|figure|footer|form|frame|frameset|h[123456]|head|header|hr|html|iframe|legend|li|link|main|menu|menuitem|meta|nav|noframes|ol|optgroup|option|p|param|section|source|title|summary|table|tbody|td|tfoot|th|thead|title|tr|track|ul)(?:\s|[/]?[>]|$)"
   (re-pattern (str "(?i)^(?:" OPENTAG "|" CLOSETAG ")\\\\s*$"))])

(def html-block-close
  [#"."
   #"(?i)<\/(?:script|pre|style)>"
   #"-->"
   #"\?>"
   #">"
   #"\]\]>"])

;; -- Utils

(defn match [re in]
  (re-find re (if-not (string? in) (str in) in)))

(defn is-blank?
  "Returns true if string contains only space characters."
  [s]
  (not (match non-space s)))

(defn space-or-tab? [c]
  (or (= c (char c/Space))
      (= c (char c/Tab))))