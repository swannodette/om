(ns om.devcards.bugs
  (:require [cljs.test :refer-macros [is async]]
            [devcards.core :refer-macros [defcard deftest]]
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
  (om/mock-root reconciler Dashboard))

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
         (om/mock-root rec Root))

(def tempid-550 (om/tempid))

(defui Item-550
       static om/Ident
       (ident [_ {:keys [db/id]}] [:by-id id])
       static om/IQuery
       (query [_] [:label :db/id])
       Object
       (render [this]
               (let [{:keys [label]} (om/props this)]
                 (dom/div nil label))))

(def item-view-550 (om/factory Item-550 {:keyfn :db/id}))

(defui App-550
       static om/IQuery
       (query [_] [{:app/items (om/get-query Item-550)}])
       Object
       (render [this]
               (let [{items :app/items} (om/props this)]
                 (apply dom/div nil
                        (dom/button #js {:onClick #(om/transact! this '[(item/consolidate) :app/items])} "consolidate")
                        "Items"
                        (map item-view-550 items)))))

(defn local-read-550 [{:keys [state query]} key params]
  (let [value (get @state key)]
    (if value
      {:value (om/db->tree query value @state)})))
(defn local-mutate-550 [_ _ _] {:remote true})
(def parser-550 (om/parser {:read local-read-550 :mutate local-mutate-550}))

(def reconciler-550
  (om/reconciler {:state  {:app/items [{:db/id tempid-550
                                        :label "sample"}]}
                  :parser parser-550
                  :id-key :db/id
                  :send   (fn [_ cb]
                            (cb {'item/consolidate {:tempids {[:by-id tempid-550] [:by-id "123"]}}}))}))

(defcard om-550
         "Test that mock-root can properly find root query"
         (om/mock-root reconciler-550 App-550))

(comment

  (require '[cljs.pprint :as pprint])

  (pprint/pprint @(om/get-indexer reconciler)))