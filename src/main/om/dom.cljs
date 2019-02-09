(ns om.dom
  (:refer-clojure :exclude [map mask meta time select])
  (:require-macros [om.dom :as dom])
  (:require [cljsjs.react]
            [cljsjs.react.dom]
            [om.util :as util]
            [goog.object :as gobj]))

(dom/gen-react-dom-fns)

(defn- update-state
  "Updates the state of the wrapped input element."
  [component next-props value]
  (let [on-change (gobj/getValueByKeys component "state" "onChange")
        next-state #js {}]
    (gobj/extend next-state next-props #js {:onChange on-change})
    (gobj/set next-state "value" value)
    (.setState component next-state)))

(defn wrap-form-element [element]
  (let [ctor (fn [props]
               (this-as this
                 (set! (.-state this)
                   (let [state #js {}]
                     (->> #js {:onChange (goog/bind (gobj/get this "onChange") this)}
                       (gobj/extend state props))
                     state))
                 (.apply js/React.Component this (js-arguments))))]
    (set! (.-displayName ctor) (str "wrapped-" element))
    (goog.inherits ctor js/React.Component)
    (specify! (.-prototype ctor)
      Object
      (onChange [this event]
        (when-let [handler (.-onChange (.-props this))]
          (handler event)
          (update-state
            this (.-props this)
            (gobj/getValueByKeys event "target" "value"))))

      (componentWillReceiveProps [this new-props]
        (let [state-value (gobj/getValueByKeys this "state" "value")
              element-value (gobj/get (js/ReactDOM.findDOMNode this) "value")]
          ;; On IE, onChange event might come after actual value of
          ;; an element have changed. We detect this and render
          ;; element as-is, hoping that next onChange will
          ;; eventually come and bring our modifications anyways.
          ;; Ignoring this causes skipped letters in controlled
          ;; components
          ;; https://github.com/facebook/react/issues/7027
          ;; https://github.com/reagent-project/reagent/issues/253
          ;; https://github.com/tonsky/rum/issues/86
          ;; TODO: Find a better solution, since this conflicts
          ;; with controlled/uncontrolled inputs.
          ;; https://github.com/r0man/sablono/issues/148
          (if (not= state-value element-value)
            (update-state this new-props element-value)
            (update-state this new-props (gobj/get new-props "value")))))

      (render [this]
        (js/React.createElement element (.-state this))))
    (js/React.createFactory ctor)))

(def input (wrap-form-element "input"))

(def textarea (wrap-form-element "textarea"))

(def option (wrap-form-element "option"))

(def select (wrap-form-element "select"))

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

(defn <>
  "Create a `React.Fragment` with either child or children using `React.createElement`"
  ([opts child]
   (<> opts child nil))
  ([opts child & more]
   (apply js/React.createElement js/React.Fragment opts child more)))
