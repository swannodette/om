(ns om.dom
  (:refer-clojure :exclude [map mask meta time select])
  (:require-macros [om.dom :as dom])
  (:require [react :as react]
            [react-dom :as react-dom]
            ["react-dom/server" :as react-dom-server]
            [om.util :as util]
            [goog.object :as gobj]))

(dom/gen-react-dom-fns)

(defn wrap-form-element [ctor display-name]
  (react/createFactory
    (react/createClass
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

(def input (wrap-form-element react/DOM.input "input"))

(def textarea (wrap-form-element react/DOM.textarea "textarea"))

(def option (wrap-form-element react/DOM.option "option"))

(def select (wrap-form-element react/DOM.select "select"))

(defn render
  "Equivalent to React.render"
  [component el]
  (react-dom/render component el))

(defn render-to-str
  "Equivalent to React.renderToString"
  [c]
  (react-dom-server/renderToString c))

(defn node
  "Returns the dom node associated with a component's React ref."
  ([component]
   (react-dom/findDOMNode component))
  ([component name]
   (some-> (.-refs component) (gobj/get name) (react-dom/findDOMNode))))

(defn create-element
  "Create a DOM element for which there exists no corresponding function.
   Useful to create DOM elements not included in React.DOM. Equivalent
   to calling `react/createElement`"
  ([tag]
   (create-element tag nil))
  ([tag opts]
   (react/createElement tag opts))
  ([tag opts & children]
   (react/createElement tag opts children)))
