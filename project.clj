(defproject org.omcljs/om "1.0.0-alpha49-SNAPSHOT"
  :description "ClojureScript interface to Facebook's React"
  :url "http://github.com/swannodette/om"
  :license {:name "Eclipse"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories [["clojars" {:sign-releases false}]]

  :jvm-opts ^:replace ["-Xms512m" "-Xmx512m" "-server"]

  :source-paths  ["src/main" "src/devcards" "src/test"]

  :dependencies [[org.clojure/clojure "1.9.0-alpha14" :scope "provided"]
                 [org.clojure/clojurescript "1.9.473" :scope "provided" :classifier "aot"
                  :exclusions [org.clojure/clojure
                               org.clojure/data.json]]
                 [org.clojure/data.json "0.2.6" :scope "provided" :classifier "aot"]
                 [cljsjs/react "15.3.1-0"]
                 [cljsjs/react-dom "15.3.1-0"]
                 [com.cognitect/transit-clj "0.8.290"]
                 [com.cognitect/transit-cljs "0.8.239"]

                 [org.clojure/core.async "0.2.385" :scope "test"
                  :exclusions [org.clojure/tools.reader]]
                 [figwheel-sidecar "0.5.6" :scope "test"
                  :exclusions [org.clojure/clojurescript
                               org.clojure/tools.reader]]
                 [devcards "0.2.1-7" :scope "test"
                  :exclusions [org.clojure/clojurescript]]]

  :plugins [[lein-cljsbuild "1.1.2"]]

  :jar-exclusions [#".DS_Store" #"dev" #"devcards" #"test" #"index.html"
                   #"main" #"public"]

  :clean-targets ^{:protect false} ["resources/out"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src/main" "src/dev"]
              :compiler {:main om.dev
                         :asset-path "out"
                         :output-to "resources/out/app.js"
                         :output-dir "resources/out"
                         :optimizations :none}}
             {:id "test"
              :source-paths ["src" "test"]
              :compiler {
                :output-to "script/tests.simple.js"
                :output-dir "script/out"
                :source-map "script/tests.simple.js.map"
                :output-wrapper false
                :optimizations :simple}}
             ;; examples
             {:id "hello"
              :source-paths ["src" "examples/hello/src"]
              :compiler {
                :output-to "examples/hello/main.js"
                :output-dir "examples/hello/out"
                :source-map true
                :optimizations :none}}
             {:id "state-bug"
              :source-paths ["src" "examples/state_bug/src"]
              :compiler {
                :main examples.state-bug.core
                :asset-path "out"
                :output-to "examples/state_bug/main.js"
                :output-dir "examples/state_bug/out"
                :source-map true
                :optimizations :none}}
             {:id "verify"
              :source-paths ["src" "examples/verify/src"]
              :compiler {
                :output-to "examples/verify/main.js"
                :output-dir "examples/verify/out"
                :source-map true
                :optimizations :none}}
             {:id "input"
              :source-paths ["src" "examples/input/src"]
              :compiler {
                :output-to "examples/input/main.js"
                :output-dir "examples/input/out"
                :source-map true
                :optimizations :none}}
             {:id "multi"
              :source-paths ["src" "examples/multi/src"]
              :compiler {
                :output-to "examples/multi/main.js"
                :output-dir "examples/multi/out"
                :source-map true
                :optimizations :none}}
             {:id "cursor-as-key"
              :source-paths ["src" "examples/cursor_as_key/src"]
              :compiler {
                :output-to "examples/cursor_as_key/main.js"
                :output-dir "examples/cursor_as_key/out"
                :source-map true
                :optimizations :none}}
             {:id "unmount"
              :source-paths ["src" "examples/unmount/src"]
              :compiler {
                :output-to "examples/unmount/main.js"
                :output-dir "examples/unmount/out"
                :source-map true
                :optimizations :none}}
             {:id "mouse"
              :source-paths ["src" "examples/mouse/src"]
              :compiler {
                :output-to "examples/mouse/main.js"
                :output-dir "examples/mouse/out"
                :source-map true
                :optimizations :none}}
             {:id "multiroot"
              :source-paths ["src" "examples/multiroot/src"]
              :compiler {
                :output-to "examples/multiroot/main.js"
                :output-dir "examples/multiroot/out"
                :source-map true
                :optimizations :none}}
             {:id "counters"
              :source-paths ["src" "examples/counters/src"]
              :compiler {
                :output-to "examples/counters/main.js"
                :output-dir "examples/counters/out"
                :source-map true
                :optimizations :none}}
             {:id "animation"
              :source-paths ["src" "examples/animation/src"]
              :compiler {
                :output-to "examples/animation/main.js"
                :output-dir "examples/animation/out"
                :source-map true
                :optimizations :none}}
             {:id "shared"
              :source-paths ["src" "examples/shared/src"]
              :compiler {
                :output-to "examples/shared/main.js"
                :output-dir "examples/shared/out"
                :source-map true
                :optimizations :none}}
             {:id "typeahead"
              :source-paths ["src" "examples/typeahead/src"]
              :compiler {
                :output-to "examples/typeahead/main.js"
                :output-dir "examples/typeahead/out"
                :source-map true
                :optimizations :none}}
             {:id "sortable"
              :source-paths ["src" "examples/sortable/src"]
              :compiler {
                :output-to "examples/sortable/main.js"
                :output-dir "examples/sortable/out"
                :source-map true
                :optimizations :none}}
             {:id "instrument"
              :source-paths ["src" "examples/instrument/src"]
              :compiler {
                :output-to "examples/instrument/main.js"
                :output-dir "examples/instrument/out"
                :source-map true
                :optimizations :none}}
             {:id "stateful"
              :source-paths ["src" "examples/stateful/src"]
              :compiler {
                :output-to "examples/stateful/main.js"
                :output-dir "examples/stateful/out"
                :source-map true
                :optimizations :none}}
             {:id "harmful"
              :source-paths ["src" "examples/harmful/src"]
              :compiler {
                :output-to "examples/harmful/main.js"
                :output-dir "examples/harmful/out"
                :source-map true
                :optimizations :none}}
             {:id "mixins"
              :source-paths ["src" "examples/mixins/src"]
              :compiler {
                :output-to "examples/mixins/main.js"
                :output-dir "examples/mixins/out"
                :source-map true
                :optimizations :none}}
             {:id "two-lists"
              :source-paths ["src" "examples/two_lists/src"]
              :compiler {
                :output-to "examples/two_lists/main.js"
                :output-dir "examples/two_lists/out"
                :source-map true
                :optimizations :none}}
             {:id "update-props"
              :source-paths ["src" "examples/update_props/src"]
              :compiler {
                :output-to "examples/update_props/main.js"
                :output-dir "examples/update_props/out"
                :source-map true
                :optimizations :none}}
             {:id "refs"
              :source-paths ["src" "examples/refs/src"]
              :compiler {
                :output-to "examples/refs/main.js"
                :output-dir "examples/refs/out"
                :source-map true
                :optimizations :none}}
             {:id "tests"
              :source-paths ["src" "examples/tests/src"]
              :compiler {
                :output-to "examples/tests/main.js"
                :output-dir "examples/tests/out"
                :source-map true
                :optimizations :none}}]})
