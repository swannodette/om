(ns examples.stateful.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [react :as react]))

(defn counter-view [data owner]
  (reify
    om/IRender 
    (render [_]
      (dom/div nil
        (dom/h2 nil "A Counter!")
        (dom/div
          #js {:onClick (fn [e] (om/transact! data :count inc))}
          (:count data))))))

(defn applyf [x korks f]
  (if (empty? korks)
    (f x)
    (update-in x korks f)))

(def MyClass*
  (react/createClass
    #js
    {:getInitialState
     (fn [] #js {:value {:count 0}})
     :render
     (fn []
       (this-as this
         (let [counter (aget (.-state this) "value")]
           (om/build counter-view 
             (specify! counter
               IDeref
               (-deref [this] this)
               om/ITransact
               (-transact! [m korks f _]
                 (.setState this #js {:value (applyf m korks f)})))))))}))

(def MyClass (react/createFactory MyClass*))

(react/renderComponent (MyClass.) (.getElementById js/document "app"))
