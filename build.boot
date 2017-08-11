(set-env!
 :source-paths    #{"src/main"}
 :dependencies '[[org.clojure/clojure         "1.9.0-alpha16"  :scope "provided"]
                 [org.clojure/clojurescript   "1.9.542"        :scope "provided"
                  :classifier "aot"
                  :exclusions [org.clojure/clojure
                               org.clojure/data.json]]
                 [org.clojure/clojurescript   "1.9.542"        :scope "provided"
                  :exclusions [org.clojure/clojure
                               org.clojure/data.json]]
                 [org.clojure/data.json       "0.2.6"          :scope "provided"
                  :classifier "aot"]
                 [cljsjs/react                "15.5.4-0"]
                 [cljsjs/react-dom            "15.5.4-0"]
                 [cljsjs/create-react-class   "15.6.0-0"]
                 [com.cognitect/transit-clj   "0.8.300"]
                 [com.cognitect/transit-cljs  "0.8.239"]

                 [org.clojure/core.async      "0.3.443"        :scope "test"
                  :exclusions [org.clojure/tools.reader]]
                 [figwheel-sidecar            "0.5.10"         :scope "test"
                  :exclusions [org.clojure/clojurescript
                               org.clojure/tools.reader]]
                 [devcards                    "0.2.3"        :scope "test"
                  :exclusions [org.clojure/clojurescript]]
                 [com.cemerick/piggieback     "0.2.1"          :scope "test"
                  :exclusions [org.clojure/clojure
                               org.clojure/tools.nrepl
                               org.clojure/clojurescript]]
                 [pandeiro/boot-http          "0.8.3"          :scope "test"]
                 [adzerk/boot-cljs            "2.0.0"          :scope "test"
                  :exclusions [org.clojure/clojurescript]]
                 [adzerk/boot-cljs-repl       "0.3.3"          :scope "test"]
                 [adzerk/boot-test            "1.2.0"          :scope "test"]
                 [crisptrutski/boot-cljs-test "0.3.0"          :scope "test"
                  :exclusions [org.clojure/clojurescript]]
                 [adzerk/boot-reload          "0.5.1"          :scope "test"]
                 [org.clojure/tools.nrepl     "0.2.13"         :scope "test"]
                 [weasel                      "0.7.0"          :scope "test"]]
 :exclusions '[org.clojure/clojure org.clojure/clojurescript])

(require
 '[adzerk.boot-cljs            :refer [cljs]]
 '[adzerk.boot-cljs-repl       :as cr :refer [cljs-repl start-repl]]
 '[adzerk.boot-reload          :refer [reload]]
 '[adzerk.boot-test            :as bt]
 '[crisptrutski.boot-cljs-test :as bct]
 '[pandeiro.boot-http          :refer [serve]])

(deftask devcards []
  (set-env! :source-paths #(conj % "src/devcards")
            :resource-paths #{"resources/public"})
  (comp
    ;; remove possible artifacts of Figwheel compilation
    (sift :include #{#"^devcards\/(out\/|main.js)"} :invert true)
    (serve :port 3449)
    (watch)
    (cljs-repl)
    (reload)
    (speak)
    (cljs :source-map true
          :compiler-options {:devcards true
                             :main 'om.devcards.core
                             :parallel-build true}
          :ids #{"devcards/main"})
    (target)))

(deftask testing []
  (set-env! :source-paths #(conj % "src/test"))
  identity)

(ns-unmap 'boot.user 'test)

(deftask test-clj []
  (comp
    (testing)
    (bt/test)))

(deftask test-cljs
  [e exit?     bool  "Enable flag."]
  (let [exit? (cond-> exit?
                (nil? exit?) not)]
    (comp
      (testing)
      (bct/test-cljs
        :js-env :node
        :namespaces #{'om.next.tests}
        :cljs-opts {:parallel-build true}
        :exit? exit?))))

(deftask test
  [e exit?     bool  "Enable flag."]
  (let [exit? (cond-> exit?
                (nil? exit?) not)]
    (comp
      (test-clj)
      (test-cljs :exit? exit?))))

(deftask auto-test []
  (comp
    (watch)
    (speak)
    (test :exit? false)))
