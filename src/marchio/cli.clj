(ns marchio.cli
  (:require
    [marchio.render :as r]
    [clojure.string :as string]
    [me.raynes.fs :as fs]
    [clojure.tools.cli :as cli])
  (:import
    (java.io BufferedReader)))

(def project-version "0.1.0")

(defn- usage [options-summary]
  (->> ["Convert CommonMark to HTML. Files are concatenated before"
        "parsing. If no files are given, input is read from stdin."
        ""
        "Usage:"
        ""
        "marchio [options] [files..]"
        ""
        "Options:"
        ""
        options-summary
        ""
        "Please refer to the README for more info: https://github.com/ff-/marchio"]
       (string/join \newline)))

(defn- join-files
  "Join multiple files into a single string"
  [filenames]
  (->> filenames
       (filterv fs/exists?)
       (mapv #(slurp % :encoding "UTF-8"))
       (string/join \newline)))

(def formats [:xml :html :ast])

(defn- formats-to-string
  "Output a string of formats for the cli"
  []
  (->> formats
       (mapv #(str "\"" (name %) "\""))
       (string/join ", ")))

(def opts
    [["-t" "--to FORMAT" (str "Specify output format: " (formats-to-string))
      :default :html
      :validate [#(some? #{(keyword %)} formats)
                 (str "Must be one of " (formats-to-string))]]
     ["-h" "--help"      "Print this help"]
     ["-v" "--version"   "Print version information"]
     [nil  "--time"      "Produce timings for phases of parsing and rendering"]
     [nil  "--smart"     "Parse 'smart' punctuation"]
     [nil  "--safe"      "Omit raw HTML and potentially unsafe attributes"]
     [nil  "--sourcepos" "Include source position attributes in HTML tags"]])

(defn- exit
  "Exits with status leaving a message"
  [status msg]
  (println msg)
  (System/exit status))

;; TODO handle the other cli-options
(defn parse-cli
  "Parse the cli arguments and render accordingly."
  [args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args opts)]

    (cond
      (:help options)       (exit 0 (usage summary))
      (:version options)    (exit 0 (str "marchio " project-version))
      (not-empty errors)    (exit 1 (doseq [err errors] (println err)))
      :else                 (-> (if (not-empty arguments)
                                  arguments
                                  ["/dev/stdin"])
                                join-files
                                println)))) ;r/md->html println))))