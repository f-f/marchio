(defproject io.ferrai/marchio "0.1.0"
  :author "Fabrizio Ferrai <https://ferrai.io>"
  :description "CommonMark parser for Clojure/Script"
  :url "https://github.com/ff-/marchio"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "Same as Clojure"}
  :min-lein-version "2.3.3"

  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.293"]
                 [org.clojure/core.async "0.2.395"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/data.xml "0.1.0-beta1"]
                 [org.clojure/tools.cli "0.3.5"]
                 [better-cond "1.0.1"]
                 [cheshire "5.6.3"]
                 [hiccup "1.0.5"]
                 [im.chit/hara.zip "2.4.8"]
                 [me.raynes/fs "1.4.6"]
                 [medley "0.8.4"]
                 [org.blancas/kern "1.0.0"]
                 [swiss-arrows "1.0.0"]
                 [uncomplicate/fluokitten "0.5.1"]]

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
             :1.9  {:dependencies [[org.clojure/clojure "1.9.0-alpha14"]]}
             :test {:dependencies [[org.clojure/test.check "0.9.0"]]
                    :plugins [[com.jakemccrary/lein-test-refresh "0.17.0"]]}
             :dev [:1.9 :test :server-jvm]
             :uberjar {:aot :all}}

  :test-paths ["test" "src"]
  :test-refresh {:notify-command ["notify-send" "-i" "/usr/share/pixmaps/idle.xpm" "-t" "5000" "Tests"]
                 :quiet true
                 :changes-only true}

  :bin {:name "marchio"}
  :clean-targets ^{:protect false} ["target"]
  :main ^:skip-aot marchio.core
  :target-path "target/%s")
