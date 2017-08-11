(ns om.devcards.bugs
  (:require [devcards.core :refer-macros [defcard deftest dom-node]]
            [cljs.test :refer-macros [is async]]
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

;; ==================
;; OM-595

(defmulti om-595-read om/dispatch)

(defmethod om-595-read :item
  [{:keys [state query parser] :as env} key _]
  (let [st @state]
    {:value (om/db->tree query (get st key) st)}))

(def om-595-state
  {:item [:item/by-id 1]
   :item/by-id {1 {:id 1
                   :title "first"
                   :next  [:item/by-id 2]}
                2 {:id 2
                   :title "second"}}})

(def om-595-reconciler
  (om/reconciler {:state  om-595-state
                  :parser (om/parser {:read om-595-read})}))

(defui OM-595-App
  static om/IQuery
  (query [this]
    '[{:item [:id :title {:next ...}]}])
  Object
    (render [this]
      (dom/div nil (str (om/props this)))))

(defcard om-595
  "Test that `index-root` doesn't blow the stack"
  (dom-node
    (fn [_ node]
      (om/add-root! om-595-reconciler OM-595-App node))))

;; ==================
;; OM-601

(defn om-601-read
  [{:keys [state] :as env} k _]
  (let [st @state]
    {:value (get st k)}))

(defn om-601-mutate
  [{:keys [state] :as env} _ _]
  {:action #(swap! state update-in [:foo :foo/val] inc)})

(def om-601-state
  {:foo {:foo/val 0}
   :bar {:bar/val 0}})

(def om-601-reconciler
  (om/reconciler {:state  om-601-state
                  :parser (om/parser {:read om-601-read :mutate om-601-mutate})}))

(defui OM-601-Foo
  static om/IQuery
  (query [this]
    [:foo/val])
  Object
  (render [this]
    (println "Render Foo")
    (dom/div nil
      (dom/p nil (str "Foo: " (:foo/val (om/props this)))))))

(def om-601-foo (om/factory OM-601-Foo))

(defui OM-601-Bar
  static om/IQuery
  (query [this]
    [:bar/val])
  Object
  (render [this]
    (println "Render Bar")
    (dom/div nil
      (dom/p nil (str "Bar: " (:bar/val (om/props this)))))))

(def om-601-bar (om/factory OM-601-Bar))

(defui OM-601-App
  static om/IQuery
  (query [this]
    [{:foo (om/get-query OM-601-Foo)}
     {:bar (om/get-query OM-601-Bar)}])
  Object
  (render [this]
    (let [{:keys [foo bar]} (om/props this)]
      (dom/div nil
        (om-601-foo foo)
        (om-601-bar bar)
        (dom/button #js {:onClick #(om/transact! this '[(increment/foo!)])}
          "Inc foo")))))

(defcard om-601-card
  "Test that `shouldComponentUpdate` doesn't render unchanged children"
  (dom-node
    (fn [_ node]
      (om/add-root! om-601-reconciler OM-601-App node))))

;; ==================
;; OM-598

(defmulti om-598-read om/dispatch)

(defmethod om-598-read :default
  [{:keys [state]} k _]
  (let [st @state]
    {:value (get st k)}))

(defui OM-598-Child
  static om/IQuery
  (query [this]
   [:bar])
  Object
  (initLocalState [_]
    {:a 1})
  (render [this]
    (let [{:keys [a]} (om/get-state this)]
      (dom/div nil
        (dom/button #js {:onClick #(om/update-state! this update :a inc)}
          (str "a: " a))))))

(def om-598-child (om/factory OM-598-Child))

(defui OM-598-App
  static om/IQuery
  (query [this]
   [:foo])
  Object
  (componentDidMount [this]
    (om/set-query! this {:query [{:child (om/get-query OM-598-Child)}]}))
  (render [this]
    (let [props (om/props this)]
      (dom/div nil
        (when-let [child-props (:child props)]
          (om-598-child child-props))))))

(def om-598-reconciler
  (om/reconciler {:state {:foo 3 :child {:bar 2}}
                  :parser (om/parser {:read om-598-read})}))

(defcard om-598
  "Test that `reindex!` works on query transitions"
  (dom-node
    (fn [_ node]
      (om/add-root! om-598-reconciler OM-598-App node))))

;; ==================
;; OM-641

(declare om-641-reconciler)

(defui OM-641-Item
  static om/Ident
  (ident [this {:keys [id]}]
    [:items/by-id id])
  static om/IQuery
  (query [this] [:id :name])
  Object
  (render [this]
    (let [{:keys [name]} (om/props this)]
      (dom/div nil
        (dom/p nil
          (str "item: " name))
        (dom/p nil
          (str "ref->components "
            (pr-str (-> @(om/get-indexer om-641-reconciler) :ref->components))))))))

(defui OM-641-Root
  static om/IQuery
  (query [this]
    [{:item (om/get-query OM-641-Item)}])
  Object
  (render [this]
    (let [{:keys [item]} (om/props this)]
      (dom/div nil
        ((om/factory OM-641-Item) item)
        (dom/button #js {:onClick #(om/transact! this '[(change/item!)])}
          "change item")))))

(defn om-641-read
  [{:keys [state query]} k params]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defn om-641-mutate
  [{:keys [state]} _ _]
  {:action (fn []
             (let [item (get @state :item)]
               (swap! state assoc :item [:items/by-id (mod (inc (second item)) 2)])))})

(def om-641-reconciler
  (om/reconciler {:state (atom {:item [:items/by-id 0]
                                :items/by-id {0 {:id 0 :name "Item 0"}
                                              1 {:id 1 :name "Item 1"}}})
                  :parser (om/parser {:read om-641-read :mutate om-641-mutate})}))

(defcard om-641-card
  (dom-node (fn [_ node]
              (om/add-root! om-641-reconciler OM-641-Root node))))

;; ==================
;; OM-644

(def om-644-app-state (atom {:children []}))

(defn om-644-read [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defn om-644-mutate [{:keys [state] :as env} key params]
  (if (= 'load key)
    {:value {:keys [:count]}
     :action #(swap! state update-in [:children] conj (rand-int 10))}
    {:value :not-found}))

(defui OM-644-Child
  Object
  (render [this]
    (dom/div nil (om/children this))))

(def om-644-child (om/factory OM-644-Child))

(defui OM-644-Root
  static om/IQuery
  (query [this]
    [:children])
  Object
  (render [this]
    (let [{:keys [children]} (om/props this)]
      (om-644-child nil
        (dom/button #js {:onClick #(om/transact! this '[(load)])}
          "Load children")
        (dom/ul nil
          (map #(dom/li nil %) children))))))

(def om-644-reconciler
  (om/reconciler
   {:state om-644-app-state
    :parser (om/parser {:read om-644-read :mutate om-644-mutate})}))

(defcard om-644-card
  (dom-node
    (fn [_ node]
      (om/add-root! om-644-reconciler OM-644-Root node))))

;; ==================
;; OM-628

(defn om-628-read
  [{:keys [state query] :as env} k _]
  (println "query" query)
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(defui om-628-Child
  static om/Ident
  (ident [this {:keys [id]}]
    [:item/by-id id])
  static om/IQueryParams
  (params [this]
    {:count 3})
  static om/IQuery
  (query [this]
    '[:id :title (:items {:count ?count})])
  Object
  (render [this]
    (let [{:keys [title items] :as props} (om/props this)]
      (dom/div nil
        (dom/h2 nil title)
        (dom/ul nil
          (map-indexed #(dom/li #js {:key %1} (name %2)) items))
        (dom/button #js {:onClick #(om/set-query!
                                     this {:query [:id :title]} (om/path this))}
          "Set this query to: [:id :title]")
        (dom/button #js {:onClick #(println "my query" (om/get-query this))}
          "Get local component query")))))

(def om-628-child (om/factory om-628-Child {:keyfn :id}))

(defui om-628-Parent
  static om/IQuery
  (query [this]
    [{:child1 (om/get-query om-628-Child)}
     {:child2 (om/get-query om-628-Child)}])
  Object
  (render [this]
    (let [{:keys [child1 child2]} (om/props this)]
      (dom/div nil
        (dom/p nil (str "Root query: " (om/get-query this)))
        (om-628-child child1)
        (om-628-child child2)))))

(defonce om-628-state
  {:child1 {:id 1 :title "Child 1" :items ["item 1" "item 2"]}
   :child2 {:id 2 :title "Child 2" :items ["item 1" "item 2"]}})

(def om-628-reconciler
  (om/reconciler {:state om-628-state
                  :parser (om/parser {:read om-628-read})}))

(defcard om-628-card
  (dom-node
    (fn [_ node]
      (om/add-root! om-628-reconciler om-628-Parent node))))

;; ==================
;; OM-676

(def om-676-data
  {:composite/item {:id 0
                   :width 400
                   :height 400
                   :color "#428BCA"
                   :children [{:id 1
                               :width 200
                               :height 200
                               :color "#9ACD32"
                               :children [{:id 3
                                           :width 100
                                           :height 100
                                           :color "#CD329A"}
                                          {:id 4
                                           :width 100
                                           :height 100
                                           :color "#32CD65"}]}
                              {:id 2
                               :width 200
                               :height 200
                               :color "#39DBBE"}]}})

(declare om-676-component)

(defui OM-676-Component
  static om/Ident
  (ident [this {:keys [id]}]
    [:component id])
  static om/IQuery
  (query [this]
    '[:id :width :height :color {:children ...}])
  Object
  (render [this]
    (let [{:keys [id width height color caption children] :as props} (om/props this)
          {:keys [active]} (om/get-state this)]
      (dom/div #js {:className (str id)
                    :onClick (fn [e]
                               (.stopPropagation e)
                               (om/update-state! this update :active not))
                    :style
                    #js {:position "relative"
                         :float "left"
                         :width width
                         :height height
                         :zIndex 2
                         :textAlign "center"
                         :backgroundColor (if active "black" color)}}
        (map om-676-component children)))))

(def om-676-component (om/factory OM-676-Component))

(defui OM-676-App
  static om/IQuery
  (query [this]
    [{:composite/item (om/get-query OM-676-Component)}])
  Object
  (render [this]
    (let [{:keys [composite/item] :as props} (om/props this)]
      (dom/div #js {:style #js {:margin "0 auto"
                                :display "table"}}
        (om-676-component item)
        (dom/div #js {:style #js {:clear "both"}})))))

(defmulti om-676-read om/dispatch)

(defmethod om-676-read :composite/item
  [{:keys [state parser query ast] :as env} k _]
  [{:keys [query state]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(def om-676-reconciler
  (om/reconciler {:state om-676-data
                  :parser (om/parser {:read om-676-read})}))

(defcard om-676-card
  (dom-node
    (fn [_ node]
      (om/add-root! om-676-reconciler OM-676-App node))))

;; ==================
;; OM-679

(def om-679-data
  {:tree {:id 0
          :counter {:value 1}
          :children [{:id 1
                      :counter {:value 2}
                      :children [{:id 2
                                  :counter {:value 3}
                                  :children []}]}
                     {:id 3
                      :counter {:value 4}
                      :children []}]}})

(declare om-679-node)

(defmulti om-679-read om/dispatch)

(defmethod om-679-read :tree
  [{:keys [state query] :as env} _ _]
  (let [st @state]
    {:value (om/db->tree query (:tree st) st)}))

(defui OM-679-Counter
  static om/IQuery
  (query [this]
    [:value])
  Object
  (initLocalState [this]
    {:val (-> (om/props this) :value)})
  (render [this]
    (let [{:keys [val]} (om/get-state this)]
      (dom/div nil
        (dom/label nil (str "Counter value at node: " val))
        (dom/button #js {:onClick #(om/update-state! this update-in [:val] inc)}
          "Inc")))))

(def om-679-counter (om/factory OM-679-Counter))

(defui OM-679-Node
  static om/Ident
  (ident [this {:keys [id]}]
    [:node/by-id id])
  static om/IQuery
  (query [this]
    `[:id {:counter ~(om/get-query OM-679-Counter)} {:children ~'...}])
  Object
  (render [this]
    (let [{:keys [id counter children]} (om/props this)]
      (dom/li nil
        (om-679-counter counter)
        (dom/ul nil
          (map om-679-node children))))))

(def om-679-node (om/factory OM-679-Node))

(defui OM-679-Tree
  static om/IQuery
  (query [this]
    [{:tree (om/get-query OM-679-Node)}])
  Object
  (render [this]
    (let [{:keys [tree]} (om/props this)]
      (dom/ul nil
        (om-679-node tree)))))

(def om-679-parser
  (om/parser {:read   om-679-read}))

(def om-679-reconciler
  (om/reconciler
    {:state   om-679-data
     :parser  om-679-parser}))

(defcard om-679-card
  (dom-node
    (fn [_ node]
      (om/add-root! om-679-reconciler OM-679-Tree node))))

;; ==================
;; OM-679-2

(def om-679-2-data
  {:tree {:node/type :tree/foo
          :id 0
          :foo/value "1"
          :children [{:node/type :tree/bar
                      :bar/value "1.1"
                      :counter {:value 1}
                      :id 1
                      :children [{:node/type :tree/bar
                                  :id 2
                                  :counter {:value 2}
                                  :bar/value "1.1.1"
                                  :children []}]}
                     {:node/type :tree/foo
                      :id 3
                      :foo/value "1.2"
                      :children []}]}})

(defui OM-679-2-Counter
  static om/IQuery
  (query [this]
    [:value])
  Object
  (initLocalState [this]
    {:val (-> (om/props this) :value)})
  (render [this]
    (let [{:keys [val]} (om/get-state this)]
      (dom/div nil
        (dom/label nil (str "Counter value at node: " val))
        (dom/button #js {:onClick #(om/update-state! this update-in [:val] inc)}
          "Inc")))))

(def om-679-2-counter (om/factory OM-679-2-Counter))

(declare om-679-2-node)

(defui OM-679-2-BarNode
  static om/IQuery
  (query [this]
    `[:id :node/type {:counter ~(om/get-query OM-679-2-Counter)} :bar/value {:children ~'...}])
  Object
  (render [this]
    (let [{:keys [bar/value counter children]} (om/props this)]
      (dom/li nil
        (dom/p nil (str "Bar value: " value))
        (om-679-2-counter counter)
        (dom/ul nil
          (map om-679-2-node children))))))

(def om-679-2-bar-node (om/factory OM-679-2-BarNode))

(defui OM-679-2-FooNode
  static om/IQuery
  (query [this]
    '[:id :node/type :foo/value {:children ...}])
  Object
  (render [this]
    (let [{:keys [foo/value children]} (om/props this)]
      (dom/li nil
        (dom/p nil (str "Foo value: " value))
        (dom/ul nil
          (map om-679-2-node children))))))

(def om-679-2-foo-node (om/factory OM-679-2-FooNode))

(defui OM-679-2-Node
  static om/Ident
  (ident [this {:keys [node/type id]}]
    [type id])
  static om/IQuery
  (query [this]
    {:tree/foo (om/get-query OM-679-2-FooNode)
     :tree/bar (om/get-query OM-679-2-BarNode)})
  Object
  (render [this]
    (let [{:keys [node/type] :as props} (om/props this)]
      (({:tree/foo om-679-2-foo-node
         :tree/bar om-679-2-bar-node} type)
         props))))

(def om-679-2-node (om/factory OM-679-2-Node))

(defui OM-679-2-Tree
  static om/IQuery
  (query [this]
    `[{:tree ~(om/get-query OM-679-2-Node)}])
  Object
  (render [this]
    (let [{:keys [tree]} (om/props this)]
      (dom/ul nil
        (om-679-2-node tree)))))

(defmulti om-679-2-read om/dispatch)

(defmethod om-679-2-read :default
  [{:keys [data] :as env} k _]
  {:value (get data k)})

(defmethod om-679-2-read :children
  [{:keys [parser data union-query state] :as env} k _]
  (let [st @state
        f #(parser (assoc env :data (get-in st %)) ((first %) union-query))]
    {:value (into [] (map f) (:children data))}))

(defmethod om-679-2-read :tree
  [{:keys [state parser query ast] :as env} k _]
  (let [st @state
        [type id :as entry] (get st k)
        data (get-in st entry)
        new-env (assoc env :data data :union-query query)]
    {:value (parser new-env (type query))}))

(def om-679-2-reconciler
  (om/reconciler {:state om-679-2-data
                  :parser (om/parser {:read om-679-2-read})}))

(defcard om-679-2
  (dom-node
    (fn [_ node]
      (om/add-root! om-679-2-reconciler OM-679-2-Tree node))))

;; ==================
;; OM-673

(defui OM-673-Child
  Object
  (render [this]
    (dom/div nil (-> this om/props :text))))

(def om-673-child (om/factory OM-673-Child))

(defui OM-673-App
  Object
  (render [this]
    (dom/div nil
      (om-673-child {:text "child 1"})
      (om-673-child {:text "child 2"}))))

(def om-673-reconciler
  (om/reconciler {:state nil}))

(defcard om-673
  (dom-node
    (fn [_ node]
      (om/add-root! om-673-reconciler OM-673-App node))))

;; ==================
;; OM-734

(def OM-734-state (atom nil))

(defn handle-click! [c update-app-state?]
  (om/update-state! c assoc :text (str "foo-" (+ 100 (rand-int 100))))
  (when update-app-state?
    (swap! OM-734-state update-in [:items] (fnil conj []) (rand-int 100))))

(defui OM-734-Child
  Object
  (initLocalState [this]
    {:text "foo"})
  (render [this]
    (dom/div nil
      (dom/p nil (om/get-state this :text))
      (dom/button #js {:onClick #(handle-click! this true)} "Local + global state")
      (dom/button #js {:onClick #(handle-click! this false)} "Just local state"))))

(def om-734-child (om/factory OM-734-Child))

(defui OM-734-Root
  Object
  (render [this]
    (let [items (:items (om/props this))]
      (dom/div nil
        (dom/ul nil
          (map (partial dom/li nil) items))
        (om-734-child)))))

(def OM-734-reconciler (om/reconciler {:state OM-734-state}))

(defcard om-734
  (dom-node
    (fn [_ node]
      (om/add-root! OM-734-reconciler OM-734-Root node))))

;; ==================
;; OM-738

(defn update-child-data!
  [this value]
  (om/transact! this `[(child/update {:newval ~value})]))

(defui OM-738-Child
  static om/IQuery
  (query [_]
    [:child/data])
  Object
  (render [this]
    (let [{:keys [child/data]} (om/props this)]
      (dom/div nil
        (dom/button #js {:onClick #(update-child-data! this "Child Clicked")} "Child Button")
        (dom/p nil (str data))))))

(def om-738-child (om/factory OM-738-Child))

(defui OM-738-Root
  static om/IQuery
  (query [_]
    [{:parent/data (om/get-query OM-738-Child)}])
  Object
  (render [this]
    (let [{:keys [parent/data]} (om/props this)]
      (dom/div nil
        (dom/button #js {:onClick #(update-child-data! this "Parent Clicked")} "Parent Button")
        (om-738-child data)))))

(defn OM-738-read
  [{:keys [state query]} key _]
  {:value (get @state key)})

(defn OM-738-mutate
  [{:keys [state]} _ {:keys [newval]}]
  {:action #(swap! state assoc-in [:parent/data :child/data] newval)})

(def OM-738-reconciler
  (om/reconciler {:state {:parent/data {:child/data "Hello"}}
                  :parser (om/parser {:read OM-738-read :mutate OM-738-mutate})
                  :remotes []}))

(defcard OM-738-card
  (dom-node
    (fn [_ node]
      (om/add-root! OM-738-reconciler OM-738-Root node))))

;; =============================================================================
;; OM-779

(defui ^:once PetListItem
  static om/Ident
  (ident [this {:keys [db/id]}]
    [:pet/by-id id])
  static om/IQuery
  (query [this]
    [:db/id :pet/name])
  Object
  (render [this]
    (dom/li #js {:onClick #((om/get-computed (om/props this) :on-click) this)}
      (:pet/name (om/props this)))))

(def pet-list-item (om/factory PetListItem))

(defui ^:once PetDetails
  static om/Ident
  (ident [this {:keys [db/id]}]
    [:pet/by-id id])
  static om/IQuery
  (query [this]
    [:db/id :pet/name :pet/favorite-toy])
  Object
  (render [this]
    (dom/div nil
      (dom/h1 nil (:pet/name (om/props this)))
      (dom/p nil (str "Favorite toy: " (:pet/favorite-toy (om/props this)))))))

(def pet-details (om/factory PetDetails))

(defui ^:once PetsPage
  static om/IQueryParams
  (params [this]
    {:selected-pet [:pet/by-id 0]})
  static om/IQuery
  (query [this]
    [{[:app/current-user '_] [{:user/pets (om/get-query PetListItem)}]}
     {'?selected-pet (om/get-query PetDetails)}])
  Object
  (render [this]
    (js/console.debug (pr-str (om/props this)))
    (let [orgs (get-in (om/props this) [:app/current-user :user/pets])
          selected-pet-ident (:selected-pet (om/get-params this))
          selected-pet (get (om/props this) selected-pet-ident)]
      (dom/div nil
        (dom/div #js {:style {:display "flex"}}
          (dom/div #js {:style {:width "300px"}}
            (dom/ul nil
              (for [org orgs]
                (pet-list-item
                  (om/computed org
                    {:on-click
                     #(om/set-query! this
                        {:params
                         {:selected-pet (om/get-ident %)}})})))))
          (dom/div nil
            (when selected-pet
              (pet-details selected-pet))))))))

(def pets-page (om/factory PetsPage))

(defui ^:once OM-779-Root
  static om/IQuery
  (query [this]
    [{::route-data (om/get-query PetsPage)}])
  Object
  (render [this]
    (pets-page (::route-data (om/props this)))))

(def om-779-state
  (atom {:app/current-user
         {:user/pets [[:pet/by-id 0]
                      [:pet/by-id 1]]}
         :pet/by-id {0 {:db/id 0
                        :pet/name "Alfred"
                        :pet/favorite-toy "Stuffed Bear"}
                     1 {:db/id 1
                        :pet/name "Jasmine"
                        :pet/favorite-toy "Squeeky Toy"}}}))

(def om-779-reconciler
  (om/reconciler
   {:state om-779-state
    :parser (om/parser {:read (fn [{:keys [state query] :as env} key _]
                                (let [st @state]
                                  {:value (om/db->tree query st st)}))})}))

(defcard test-query-setting
  (dom-node
   (fn [_ node]
     (om/add-root! om-779-reconciler OM-779-Root node))))

(defcard om-799-card
  (dom/svg #js {:viewBox "0 0 400 100"}
    (dom/rect #js {:id "rectangle" :x "100" :y "20"
                   :width "50" :height "50" :fill "#29e"})
    (dom/create-element "use" #js {:xlinkHref "#rectangle"
                                   :x "150"})))

(def om-862-state (atom {:count 1}))
(def om-862-reconciler
  (om/reconciler {:state   om-862-state
                  :remotes [:remote]
                  :parser  (om/parser
                             {:read   (fn [{:keys [state]} key _]
                                        {:value (get @state key)})
                              :mutate (fn [{:keys [state]} key _]
                                        {:value  {}
                                         :action #(swap! state
                                                         update :count inc)
                                         :remote (= 'count/inc-by-each-remote
                                                    key)})})
                  :send    (fn [{:keys [remote]} cb]
                             (when remote
                               (let [resp {:count (inc (:count @om-862-state))}
                                     query [:count]]
                                 (cb resp query remote))))}))

(defui om-862-Root
  static om/IQuery
  (query [this]
    [:count])
  Object
  (render [this]
    (let [props (om/props this)]
      (dom/div nil
               (dom/div
                 nil
                 "Clicking first on <Should increment by 2> should not
                 prevent <Should increment by 1> from scheduling a
                 reconcile! Try clicking first on <Should increment by 2> and
                 then try clicking on <Should increment by 1>.")
               (dom/button
                 #js {:onClick #(om/transact!
                                  this '[(count/inc-by-each-remote)])}
                 "Should increment by 2")
               (dom/button
                 #js {:onClick #(om/transact!
                                  this '[(count/inc-only-client)])}
                 "Should increment by 1")
               (dom/div nil (:count props))))))
; Override om/*raf* to force the race condition that causes the abberant
; om-862 behavior
(set! om/*raf* (fn [f]
                 (println "*raf* override called")
                 (f)))
(defcard om-862-card
  (dom-node
    (fn [_ node]
      (om/add-root! om-862-reconciler om-862-Root node))))

(comment

  (require '[cljs.pprint :as pprint])

  (pprint/pprint @(om/get-indexer reconciler))

  )
