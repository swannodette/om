(ns om.dom
  (:refer-clojure :exclude [map meta time select])
  (:require-macros [om.dom :as dom])
  (:require [cljsjs.react]
            [cljsjs.react.dom]
            [om.util :as util]
            [goog.object :as gobj]))

(dom/gen-react-dom-fns)

(defn wrap-form-element [ctor display-name]
  (js/React.createFactory
    (js/React.createClass
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
