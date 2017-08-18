(ns examples.mixins.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [react :as react])
  (:import [goog.ui IdGenerator]))

(enable-console-print!)

(def TestMixin
  #js {:componentWillMount
       (fn []
         (println "TextMixin componentWillMount"))})

(def MyComponent
  (let [obj (om/specify-state-methods! (clj->js om/pure-methods))]
    (aset obj "mixins" #js [TestMixin])
    (react/createClass obj)))

(om/root
  (fn [app owner]
    (om/component
      (MyComponent. nil
        (dom/div nil "Hello world!"))))
  {}
  {:target (.getElementById js/document "app")})
