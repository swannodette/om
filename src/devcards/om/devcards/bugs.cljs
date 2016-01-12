(ns om.devcards.bugs
  (:require-macros [devcards.core :refer [defcard deftest dom-node]])
  (:require [cljs.test :refer-macros [is async]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(def init-data
  {:dashboard/posts
   [{:id 0 :favorites 0}]})

(defui Post
  static om/Ident
  (ident [this {:keys [id]}]
    [:post/by-id id])

  static om/IQuery
  (query [this]
    [:id :favorites])

  Object
  (render [this]
    (let [{:keys [id favorites] :as props} (om/props this)]
      (dom/div nil
        (dom/p nil "Favorites: " favorites)
        (dom/button
          #js {:onClick
               (fn [e]
                 (om/transact! this
                   `[(post/favorite {:id ~id})]))}
          "Favorite!")))))

(def post (om/factory Post))

(defui Dashboard
  static om/IQuery
  (query [this]
    `[({:dashboard/posts ~(om/get-query Post)} {:foo "bar"})])

  Object
  (render [this]
    (let [{:keys [dashboard/posts]} (om/props this)]
      (apply dom/ul nil
        (map post posts)))))

(defmulti read om/dispatch)

(defmethod read :dashboard/posts
  [{:keys [state query]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defmulti mutate om/dispatch)

(defmethod mutate 'post/favorite
  [{:keys [state]} k {:keys [id]}]
  {:action
   (fn []
     (swap! state update-in [:post/by-id id :favorites] inc))})

(def reconciler
  (om/reconciler
    {:state  init-data
     :parser (om/parser {:read read :mutate mutate})}))

(defcard test-om-466
  "Test that Parameterized joins work"
  (dom-node
    (fn [_ node]
      (om/add-root! reconciler Dashboard node))))

;; ==================
;; OM-552

(defui Child
  Object
  (componentWillUpdate [this next-props _]
    (.log js/console "will upd" (clj->js (om/props this)) (clj->js next-props)))
  (render [this]
    (let [{:keys [x y]} (om/props this)]
      (dom/p nil (str "x: " x "; y: " y)))))

(def child (om/factory Child))

(defui Root
  Object
  (initLocalState [_]
    {:x 0 :y 0 :pressed? false})
  (mouse-down [this e]
    (om/update-state! this assoc :pressed? true)
    (.mouse-move this e))
  (mouse-move [this e]
    (when-let [pressed? (-> this om/get-state :pressed?)]
      (om/update-state! this assoc :x (.-pageX e) :y (.-pageY e))))
  (render [this]
    (let [{:keys [x y]} (om/get-state this)]
      (dom/div #js {:style #js {:height 200
                                :width 600
                                :backgroundColor "red"}
                    :onMouseDown #(.mouse-down this %)
                    :onMouseMove #(.mouse-move this %)
                    :onMouseUp #(om/update-state! this assoc :pressed? false :x 0 :y 0)}
        (child {:x x :y y})))))

(def rec (om/reconciler {:state {}
                         :parser (om/parser {:read read})}))

(defcard test-om-552
  "Test that componentWillUpdate receives updated next-props"
  (dom-node
    (fn [_ node]
      (om/add-root! rec Root node))))

;; ==================
;; OM-543

(def om-543-data
  {:tree {:node/type :tree/foo
          :id 0
          :foo/value "1"
          :children [{:node/type :tree/bar
                      :bar/value "1.1"
                      :id 1
                      :children [{:node/type :tree/bar
                                  :id 2
                                  :bar/value "1.1.1"
                                  :children []}]}
                     {:node/type :tree/foo
                      :id 3
                      :foo/value "1.2"
                      :children []}]}})

(declare item-node)

(defui UnionBarNode
  static om/IQuery
  (query [this]
    '[:id :node/type :bar/value {:children ...}])
  Object
  (render [this]
    (let [{:keys [bar/value children]} (om/props this)]
      (dom/li nil
        (dom/p nil (str "Bar value: " value))
        (dom/ul nil
          (map item-node children))))))

(def bar-node (om/factory UnionBarNode))

(defui UnionFooNode
  static om/IQuery
  (query [this]
    '[:id :node/type :foo/value {:children ...}])
  Object
  (render [this]
    (let [{:keys [foo/value children]} (om/props this)]
      (dom/li nil
        (dom/p nil (str "Foo value: " value))
        (dom/ul nil
          (map item-node children))))))

(def foo-node (om/factory UnionFooNode))

(defui ItemNode
  static om/Ident
  (ident [this {:keys [node/type id]}]
    [type id])
  static om/IQuery
  (query [this]
    {:tree/foo (om/get-query UnionFooNode)
     :tree/bar (om/get-query UnionBarNode)})
  Object
  (render [this]
    (let [{:keys [node/type] :as props} (om/props this)]
      (({:tree/foo foo-node
         :tree/bar bar-node} type)
         props))))

(def item-node (om/factory ItemNode))

(defui UnionTree
  static om/IQuery
  (query [this]
    `[{:tree ~(om/get-query ItemNode)}])
  Object
  (render [this]
    (let [{:keys [tree]} (om/props this)]
      (dom/ul nil
        (item-node tree)))))

(defmulti om-543-read om/dispatch)

(defmethod om-543-read :default
  [{:keys [data] :as env} k _]
  {:value (get data k)})

(defmethod om-543-read :children
  [{:keys [parser data union-query state] :as env} k _]
  (let [st @state
        f #(parser (assoc env :data (get-in st %)) ((first %) union-query))]
    {:value (into [] (map f) (:children data))}))

(defmethod om-543-read :tree
  [{:keys [state parser query ast] :as env} k _]
  (let [st @state
        [type id :as entry] (get st k)
        data (get-in st entry)
        new-env (assoc env :data data :union-query query)]
    {:value (parser new-env (type query))}))

(def om-543-reconciler
  (om/reconciler {:state om-543-data
                  :parser (om/parser {:read om-543-read})}))

(defcard om-543
  "Test that recursive queries in unions work"
  (dom-node
    (fn [_ node]
      (om/add-root! om-543-reconciler UnionTree node))))

(def composite-data
  {:composite/item {:id 0
                    :width 400
                    :height 400
                    :color "#428BCA"
                    :comp/value 0
                    :children [{:id 1
                                :width 200
                                :height 200
                                :color "#9ACD32"
                                :comp/value 0
                                :children [{:id 3
                                            :width 100
                                            :height 100
                                            :color "#CD329A"
                                            :leaf/value 0}
                                           {:id 4
                                            :width 100
                                            :height 100
                                            :color "#32CD65"
                                            :leaf/value 0}]}
                               {:id 2
                                :width 200
                                :height 200
                                :color "#39DBBE"
                                :leaf/value 0}]}})

(declare component)

(defn display-id [this]
  (let [{:keys [id children] :as props} (om/props this)
        type (if children :composite :leaf)]
    (dom/div #js {:style #js {:position "absolute"
                              :textAlign "right"
                              :bottom 0
                              :zIndex 1
                              :right 5}}
      (dom/span nil
        (str "id: "id " value: " (if (= :composite type) (:comp/value props) (:leaf/value props)))
        (dom/button #js {:onClick #(om/transact! this
                                     `[(comp/inc ~{:id id :type type})])}
          "+")))))

(defn common-div [this props & children]
  (let [{:keys [id width height color value]} props]
    (dom/div #js {:className (str id)
                  :style
                    #js {:position "relative"
                         :float "left"
                         :width width
                         :height height
                         :zIndex 2
                         :textAlign "center"
                         :backgroundColor color}}
      children
      (display-id this))))

(defui Composite
  static om/Ident
  (ident [this {:keys [id children]}]
    (if-not (nil? children)
      [:composite id]
      [:leaf id]))
  static om/IQuery
  (query [this]
    '[:id :width :height :color :comp/value {:children ...}])
  Object
  (render [this]
    (let [{:keys [children] :as props} (om/props this)]
      (common-div this props (map component children)))))

(def composite (om/factory Composite))

(defui Leaf
  static om/IQuery
  (query [this]
    '[:id :width :height :color :leaf/value])
  Object
  (render [this]
    (common-div this (om/props this))))

(def leaf (om/factory Leaf))

(defui Component
  static om/Ident
  (ident [this {:keys [id children]}]
    (if-not (nil? children)
      [:composite id]
      [:leaf id]))
  static om/IQuery
  (query [this]
    {:leaf (om/get-query Leaf)
     :composite (om/get-query Composite)})
  Object
  (render [this]
    (let [{:keys [id] :as props} (om/props this)
          [type id] (om/get-ident this)]
      (({:composite composite
         :leaf leaf} type) props))))

(def component (om/factory Component))

(defui CompositeApp
  static om/IQuery
  (query [this]
    [{:composite/item (om/get-query Component)}])
  Object
  (render [this]
    (let [{:keys [composite/item]} (om/props this)]
      (dom/div #js {:style #js {:margin "0 auto"
                                :display "table"}}
        (component item)
        (dom/div #js {:style #js {:clear "both"}})))))

(defmulti composite-read om/dispatch)

(defmethod composite-read :default
  [{:keys [data] :as env} k _]
  {:value (get data k)})

(defmethod composite-read :children
  [{:keys [parser data union-query state] :as env} k _]
  (let [st @state
        f #(parser (assoc env :data (get-in st %)) ((first %) union-query))]
    {:value (into [] (map f) (:children data))}))

(defmethod composite-read :composite/item
  [{:keys [state parser query ast] :as env} k _]
  (let [st @state
        [type id :as entry] (get st k)
        data (get-in st entry)
        new-env (assoc env :data data :union-query query)]
    {:value (parser new-env (type query))}))

(defmulti composite-mutate om/dispatch)

(defmethod composite-mutate 'comp/inc
  [{:keys [state target]} k {:keys [id type]}]
  (let [val (if (= type :composite) :comp/value :leaf/value)]
    {:action #(swap! state update-in [type id val] inc)}))

(def composite-reconciler
  (om/reconciler {:state composite-data
                  :parser (om/parser {:read composite-read :mutate composite-mutate})}))

(defcard om-572
  "Test that recursive queries in unions work"
  (dom-node
    (fn [_ node]
      (om/add-root! composite-reconciler CompositeApp node))))

(comment

  (require '[cljs.pprint :as pprint])

  (pprint/pprint @(om/get-indexer reconciler))

  )