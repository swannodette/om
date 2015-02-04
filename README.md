# Om

A [ClojureScript](http://github.com/clojure/clojurescript) interface
to [Facebook's React](http://facebook.github.io/react/).

Om allows users to represent their UIs simply as
[EDN](http://github.com/edn-format/edn). Because ClojureScript data is
immutable data, Om can always rapidly re-render the UI from the
root. Thus Om UIs are out of the box snapshotable and undoable and
these operations have no implementation complexity and little
overhead.

[See](http://swannodette.github.io/todomvc/labs/architecture-examples/om-undo/index.html)
for [yourself](http://swannodette.github.io/2013/12/31/time-travel/).

## Unique Features

Om supports features not currently present in React:

* Global state management facilities built in
* Components may have arbitrary data dependencies, not limited to props & state
* Component construction can be intercepted via
  `:instrument`. Simplifies debugging components and generic editors.
* Provides stream of all application state change deltas via
  `:tx-listen`. Simplifies synchronization online and offline.
* Customizable semantics. Fine grained control over how components store
  state, even for components outside of your control. Simplifies using
  Om components outside the Om framework, debugging, and adding event
  hooks not anticipated by original component designer.

## Tutorials

There is an in-depth tutorial that will introduce you to the core
concepts of Om
[here](http://github.com/swannodette/om/wiki/Basic-Tutorial) and a
real-world integration example
[here](http://github.com/swannodette/om/wiki/Intermediate-Tutorial). The
community maintained [om-cookbook](https://github.com/omcljs/om-cookbook)
covers many common idioms and patterns.

## Examples

```clojure
(ns example
  (:require [om.core :as om]
            [om.dom :as dom]))

(defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/h1 nil (:text data)))))

(om/root widget {:text "Hello world!"}
  {:target (. js/document (getElementById "my-app"))})
```

The repo includes several simple examples you can build yourself. If
you view the `project.clj` you will see their build
identifiers. Assuming you have [Leiningen](http://leiningen.org/)
installed, to build an example run:

```
lein cljsbuild once <build-id>
```

Then open the corresponding `index.html` in your favorite browser.

For a more fleshed-out example, please see the Om implementation of
[TodoMVC](http://todomvc.com)
[exists here](http://github.com/swannodette/todomvc/blob/gh-pages/labs/architecture-examples/om/src/todomvc/app.cljs).

## Documentation

There is documentation [here](http://github.com/swannodette/om/wiki/Documentation).

There is also a
[conceptual overview](http://github.com/swannodette/om/wiki/Conceptual-overview)
that we recommend reading as there are some design choices in Om that
make it quite different from other client side solutions and even
React itself.

## Reusable Components

Om emphasizes building modular and adaptable components. Some
examples:

* [om-bootstrap](https://github.com/racehub/om-bootstrap), Bootstrap 3 Om Components
* [ankha](http://github.com/noprompt/ankha), an EDN inspector view
* [om-draggable](https://github.com/sgrove/om-draggable), generic
  draggable
* [om-autocomplete](https://github.com/arosequist/om-autocomplete),
  customizable autocompleter
* [ff-om-draggable](https://github.com/neo/ff-om-draggable)
* [om-widgets](https://bitbucket.org/athieme/om-widgets)
* [om-dev-component](https://github.com/ioRekz/om-dev-component), add dev features (e.g. state history navigation) to your component
* [om-sync](http://github.com/swannodette/om-sync), keep client and
  server in sync (experimental)

## Applications built with Om

* [HackerSchool Community](https://github.com/hackerschool/community)
* [Framed](http://www.framed.io/)
* [Netrunner](https://github.com/mtgred/netrunner)
* [CircleCI](http://www.circleci.com/), source [here](https://github.com/circleci/frontend)
* [Prismatic](http://www.getprismatic.com/)
* [Precursor](https://prcrsr.com)
* [Assistant](https://github.com/29decibel/assistant)
* [Fitsme](http://fitsmeapp.com)
* [Goya](http://jackschaedler.github.io/goya/), pixel editor with
  undo/redo and visual history
* [Coils](https://github.com/zubairq/coils), a Clojure web framework
* [wordsmith](http://wordsmith.variadic.me), a markdown editor
* [omchaya](http://github.com/sgrove/omchaya)
* [BVCA Private Equity Map](http://bvca.clustermap.trampolinesystems.com/)
* [session](http://github.com/kovasb/session)
* [pOModoro](http://pomodoro.trevorlandau.net)
* [Dakait](http://github.com/verma/dakait), a web-based tool to manage
  downloads
* [Mega Super Mario World](http://github.com/city41/mario-review), a detailed review of the classic video game and a SNES video editor

## Using it

The current version depends on React 0.12.2.

Make sure you have [Leiningen](http://leiningen.org/) installed.

Your `project.clj` should include something like the following:

```clojure
(defproject foo "0.1.0"
  ...
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2760"]
                 [org.omcljs/om "0.8.8"]]
  ...)
```

### React with Add-Ons

If you would rather use React with Add-Ons you can configure this
with Maven's exclusions feature:

```clojure
(defproject foo "0.1.0"
  ...
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2760"]
                 [org.omcljs/om "0.8.8" :exclusions [cljsjs/react]]
                 [cljsjs/react-with-addons "0.12.2-4"]]
  ...)
```

### Build configuration

For local development your
[lein-cljsbuild](http://github.com/emezeske/lein-cljsbuild) settings
should look something like this:

```clojure
:cljsbuild {
  :builds [{:id "dev"
            :source-paths ["src"]
            :compiler {
              :main main.core
              :output-to "main.js"
              :output-dir "out"
              :optimizations :none
              :source-map true}}]}
```

Your markup should look something like the following:

```html
<html>
    <body>
       <div id="my-app"></div>
       <script src="main.js" type="text/javascript"></script>
    </body>
</html>
```

For production your [lein-cljsbuild](http://github.com/emezeske/lein-cljsbuild) settings should look something
like this:

```clojure
:cljsbuild {
  :builds [{:id "release"
            :source-paths ["src"]
            :compiler {
              :main main.core
              :output-to "main.js"
              :optimizations :advanced
              :pretty-print false}}]}
```

## Contributing

Please contact me via email to request an electronic Contributor
Agreement. Once your electronic CA has been signed and returned to me
I will accept pull requests.

## FAQ

### Can I use a different HTML Syntax?

Om is not opinionated about HTML syntax, third parties can provide the
preferred flavors over the `React.DOM` api. Alternative syntaxes will
be listed here:

* [sablono](http://github.com/r0man/sablono), Hiccup-style
* [kioo](http://github.com/ckirkendall/kioo), Enlive-style

### Does Om provide routing?

Om does not ship with a router and is unlikely to. However
ClojureScript routing libraries exist that handle this problem quite
well:

* [secretary](http://github.com/gf3/secretary)
* [silk](http://github.com/DomKM/silk)
* [bidi](http://github.com/juxt/bidi)

### How do I test Om programs?

* Sean Grove's [omchaya](http://github.com/sgrove/omchaya) is a good
  starting point for understanding common testing patterns
* There are some notes [here](http://github.com/swannodette/om/wiki/Testing)

## References

* [Worlds: Controlling the Scope of Side Effects](http://www.vpri.org/pdf/tr2011001_final_worlds.pdf)
* [A Functional I/O System](http://www.ccs.neu.edu/racket/pubs/icfp09-fffk.pdf)
* [Directness and Liveness in the Morphic User Interface Construction Environment](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.103.600&rep=rep1&type=pdf)
* [Learnable Programming](http://worrydream.com/LearnableProgramming/)

## Copyright and license

Copyright © 2013-2015 David Nolen

Licensed under the EPL (see the file epl.html).
