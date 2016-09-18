(ns marchio.core
  (:require
    [marchio.cli :refer [parse-cli]])
  (:gen-class))

(defn -main
  "Parse the cli arguments, and convert input files to the desired format."
  [& args]
  (parse-cli args))