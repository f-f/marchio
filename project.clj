(defproject io.ferrai/marchio "0.1.0"
  :author "Fabrizio Ferrai <https://ferrai.io>"
  :description "CommonMark parser for Clojure/Script"
  :url "https://github.com/ff-/marchio"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "Same as Clojure"}
  :min-lein-version "2.3.3"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.async "0.2.391"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/data.xml "0.1.0-beta1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [cheshire "5.6.3"]
                 [com.taoensso/truss "1.3.6"]
                 [hiccup "1.0.5"]
                 [me.raynes/fs "1.4.6"]]

  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-codox "0.10.0"]
            [lein-bin "0.3.5"]]

  :aliases {"build-once" ["cljsbuild" "once"]
            "deploy-lib" ["do" "build-once," "deploy" "clojars," "install"]}

  :cljsbuild {:test-commands {"node"    ["node" :node-runner "target/main.js"]
                              "phantom" ["phantomjs" :runner "target/main.js"]}
              :builds [{:id :main
                        :source-paths ["src" "test"]
                        :compiler     {:output-to "target/main.js"
                                       :optimizations :advanced
                                       :main marchio.core
                                       :pretty-print false}}]}

  :profiles {:server-jvm {:jvm-opts ^:replace ["-server"]}
             :1.8  {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9  {:dependencies [[org.clojure/clojure "1.9.0-alpha12"]]}
             :test {:dependencies [[org.clojure/test.check "0.9.0"]]}
             :dev [:1.9 :test :server-jvm]
             :uberjar {:aot :all}}

  :test-paths ["test" "src"]

  :bin {:name "marchio"}
  :clean-targets ^{:protect false} ["target"]
  :main ^:skip-aot marchio.core
  :target-path "target/%s")