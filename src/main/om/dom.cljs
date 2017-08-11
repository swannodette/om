(ns om.dom
  (:refer-clojure :exclude [map mask meta time select])
  (:require-macros [om.dom :as dom])
  (:require [cljsjs.react]
            [cljsjs.react.dom]
            [cljsjs.create-react-class]
            [om.util :as util]
            [goog.object :as gobj]))

(defonce create-class
  (cond (exists? js/createReactClass) js/createReactClass
        (exists? js/require) (or (js/require "create-react-class")
                                 (throw (js/Error. "require('create-react-class') failed")))
        :else (throw (js/Error. "js/createReactClass is missing"))))

(dom/gen-react-dom-fns)

(defn wrap-form-element [ctor display-name]
  (js/React.createFactory
    (create-class
      #js
      {:getDisplayName
       (fn [] display-name)
       :getInitialState
       (fn []
         (this-as this
           #js {:value (aget (.-props this) "value")}))
       :onChange
       (fn [e]
         (this-as this
           (let [handler (aget (.-props this) "onChange")]
             (when-not (nil? handler)
               (handler e)
               (.setState this #js {:value (.. e -target -value)})))))
       :componentWillReceiveProps
       (fn [new-props]
         (this-as this
           (.setState this #js {:value (aget new-props "value")})))
       :render
       (fn []
         (this-as this
           ;; NOTE: if switch to macro we remove a closure allocation
           (let [props #js {}]
             (gobj/extend props (.-props this)
               #js {:value (aget (.-state this) "value")
                    :onChange (aget this "onChange")
                    :children (aget (.-props this) "children")})
             (ctor props))))})))

(def input (wrap-form-element js/React.DOM.input "input"))

(def textarea (wrap-form-element js/React.DOM.textarea "textarea"))

(def option (wrap-form-element js/React.DOM.option "option"))

(def select (wrap-form-element js/React.DOM.select "select"))

(defn render
  "Equivalent to React.render"
  [component el]
  (js/ReactDOM.render component el))

(defn render-to-str
  "Equivalent to React.renderToString"
  [c]
  (js/ReactDOMServer.renderToString c))

(defn node
  "Returns the dom node associated with a component's React ref."
  ([component]
   (js/ReactDOM.findDOMNode component))
  ([component name]
   (some-> (.-refs component) (gobj/get name) (js/ReactDOM.findDOMNode))))

(defn create-element
  "Create a DOM element for which there exists no corresponding function.
   Useful to create DOM elements not included in React.DOM. Equivalent
   to calling `js/React.createElement`"
  ([tag]
   (create-element tag nil))
  ([tag opts]
   (js/React.createElement tag opts))
  ([tag opts & children]
   (js/React.createElement tag opts children)))
