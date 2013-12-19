# Om

A [ClojureScript](http://github.com/clojure/clojurescript) interface
to [Facebook's React](http://facebook.github.io/react/).

Om allows users to represent their UIs simply as
[EDN](http://github.com/edn-format/edn). Because ClojureScript data is
immutable data, Om can always rapidly re-render the UI from the
root. Thus Om UIs are out of the box snapshotable and undoable and
these operations have no implementation complexity and little
overhead.

## Example

```clj
(ns example
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn widget [data]
  (om/component
    (dom/div nil "Hello world!")))

(om/root {} widget js/document.body)
```

An implementation of [TodoMVC](http://todomvc.com) implemented in Om
[exists here](http://github.com/swannodette/todomvc/blob/gh-pages/labs/architecture-examples/om/src/todomvc/app.cljs).

## Using it

Om is pre-alpha software. You need to clone ClojureScript from
master and install it via `script/build`. Then clone the Om repo
and install it locally with `lein install`.

Specify dependencies as you typically would, but ensure versions match what you find in ~/.m2/respository

For example:

```clj
:dependencies [[org.clojure/clojure "1.5.1"]
               [org.clojure/clojurescript "0.0-2127"]
               [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
               [om "0.1.0-SNAPSHOT"]]
```

For local development your [lein-cljsbuild](http://github.com/emezeske/lein-cljsbuild) settings should look something like
this:

```clj
:cljsbuild { 
  :builds [{:id "dev"
            :source-paths ["src"]
            :compiler {
              :output-to "main.js"
              :output-dir "out"
              :optimizations :none
              :source-map true
              :externs ["om/externs/react.js"]}}]}
```

Your local development markup should include something like the following:

```html
<script src="http://fb.me/react-0.5.1.js"></script>
<script src="out/goog/base.js" type="text/javascript"></script>
<script src="main.js" type="text/javascript"></script>
<script type="text/javascript">goog.require("main.core");</script>
```

For production your [lein-cljsbuild](http://github.com/emezeske/lein-cljsbuild) settings should look something
like this:

```clj
:cljsbuild { 
  :builds [{:id "release"
            :source-paths ["src"]
            :compiler {
              :output-to "main.js"
              :optimizations :advanced
              :pretty-print false
              :preamble ["om/react.min.js"]
              :externs ["om/externs/react.js"]
              :closure-warnings
              {:non-standard-jsdoc :off}}}]}
```

This will generate a single file `main.js`.
