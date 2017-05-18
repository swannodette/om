(ns om.next.tests
  #?(:clj (:refer-clojure :exclude [read]))
  (:require #?@(:cljs [[cljsjs.react]])
            [clojure.test :refer [deftest is are testing run-tests]]
            [clojure.zip :as zip]
            [om.next :as om #?(:clj :refer :cljs :refer-macros) [defui ui]]
            [om.next.protocols :as p]
            [om.next.impl.parser :as parser]
            [om.tempid :refer [tempid]]
            [om.util :as util]))

;; -----------------------------------------------------------------------------
;; Predicates

(deftest test-expr-predicates
  (is (util/join? '{:foo [*]}))
  (is (util/join? '({:foo [*]} {:woz 0})))
  (is (util/union? '{:foo {:bar [*] :baz [*]}}))
  (is (util/union? '({:foo {:bar [*] :baz [*]}} {:woz 0})))
  (is (util/mutation? '(foo/bar {:baz :woz}))))

;; -----------------------------------------------------------------------------
;; Components

(defui ^:once Component
  static om/IQuery
  (query [this]
    '[:foo/bar :baz/woz]))

(def component (om/factory Component))

(defui ComponentList
  static om/IQueryParams
  (params [this]
    {:component (om/get-query Component)})
  static om/IQuery
  (query [this]
    '[{:components/list ?component} :app/title]))

(def component-list (om/factory ComponentList))

(defui ComponentWithParams
  static om/IQueryParams
  (params [this]
    {:some/param 42})
  static om/IQuery
  (query [this]
    '[{:some/key ?some/param} :app/title]))

(deftest test-component?
  (is (om/component? #?(:clj  (Component (atom nil) (atom nil) nil nil)
                        :cljs (Component. {})))))

(deftest test-pr-str-component
  (is (= (pr-str Component) "om.next.tests/Component")))

(deftest test-construction
  (let [c (component-list {:foo/bar 1})]
    (is (= (om/react-type c) ComponentList))
    #?(:cljs (is (= (om/t c) 0)))))

;; -----------------------------------------------------------------------------
;; Anonymous Components

(defn make-ui-class []
  (ui
    static om/IQuery
    (query [this]
      '[:foo/bar :baz/woz])))

(deftest test-get-query-anon-ui
  (let [q (om/get-query (make-ui-class))]
    (is (= [:foo/bar :baz/woz] q))))

;; -----------------------------------------------------------------------------
;; Queries

(deftest test-query
  (is (= #?(:clj  ((-> Component meta :query) Component)
            :cljs (om/query Component))
         '[:foo/bar :baz/woz]))
  (is (= #?(:clj  ((-> ComponentList meta :query) ComponentList)
            :cljs (om/query ComponentList))
         '[{:components/list ?component} :app/title])))

(deftest test-get-query
  (is (= (om/get-query Component)
         '[:foo/bar :baz/woz]))
  (is (= (om/get-query ComponentList)
         '[{:components/list [:foo/bar :baz/woz]} :app/title]))
  (is (= (om/get-query ComponentWithParams)
         '[{:some/key 42} :app/title])))

(deftest test-focus-query
  (is (= (om/focus-query [:foo/bar] [])
         [:foo/bar]))
  (is (= (om/focus-query
           [:foo/bar {:baz/woz [:goz/noz]}]
           [:baz/woz])
         [{:baz/woz [:goz/noz]}]))
  (is (= (om/focus-query
           [:foo/bar :baz/woz]
           [:baz/woz])
        [:baz/woz]))
  (is (= (om/focus-query
           [:foo/bar {:baz/woz [:goz/noz {:bop/wop [:nop/sop]} :cuz/wuz]}]
           [:baz/woz :bop/wop])
        [{:baz/woz [{:bop/wop [:nop/sop]}]}]))
  (is (= (om/focus-query
           '[{:tree [:id {:counter [:value]} {:children ...}]}]
           [:tree :children :counter])
        [{:tree [{:children [{:counter [:value]}]}]}]))
  (is (= (om/focus-query
           '[{:tree
              {:tree/foo [:id :node/type :foo/value {:children ...}]
               :tree/bar [:id :node/type {:counter [:value]} :bar/value {:children ...}]}}]
           [:tree :tree/foo :children :tree/foo])
         '[{:tree {:tree/foo [{:children {:tree/foo [:id :node/type :foo/value {:children ...}]}}]}}]))
  (is (= (om/focus-query
           '[{:tree
              {:tree/foo [:id :node/type :foo/value {:children ...}]
               :tree/bar [:id :node/type {:counter [:value]} :bar/value {:children ...}]}}]
           [:tree :tree/foo :children :tree/bar])
         '[{:tree
            {:tree/foo [{:children {:tree/bar [:id :node/type {:counter [:value]}
                                               :bar/value {:children ...}]}}]}}]))
  (is (= (om/focus-query
           '[{:tree
              {:tree/foo [:id :node/type :foo/value {:children ...}]
               :tree/bar [:id :node/type {:counter [:value]} :bar/value {:children ...}]}}]
           [:tree :tree/foo :children :tree/bar :counter])
         '[{:tree
            {:tree/foo [{:children {:tree/bar [{:counter [:value]}]}}]}}])))

(deftest test-focus->path
  (is (= (#'om/focus->path [{:baz/woz [{:bop/wop [:nop/sop]}]}])
         [:baz/woz :bop/wop]))
  (is (= (#'om/focus->path [:app/title {:counters/list [:db/id :counter/count]}])
         []))
  (is (= (#'om/focus->path [{:todos/list [{[:todo/by-id 0] [:id :title]}]}])
         [:todos/list [:todo/by-id 0]]))
  (is (= (#'om/focus->path [{:todos/list [{'[:current-todo _] [:id :title]}]}])
         [:todos/list '[:current-todo _]]))
  (is (= (#'om/focus->path [{:people/list [{[:person/by-id 0] [{:person/name [:name/first :name/last]}]}]}])
         [:people/list [:person/by-id 0] :person/name]))
  (is (= (#'om/focus->path [{:people/list [{'[:current-user _] [:name/first :name/last]}]}] [:people/list '[:current-user _]])
         [:people/list '[:current-user _]])))

(comment
  (om/focus-query '[{:tree [:id :node-value {:children ...}]}]
    [:tree :children])
  )

(deftest test-om-423
  (is (= (om/focus-query '[:foo ({:people [:name :age]} {:length 3}) :bar] [:people])
         '[({:people [:name :age]} {:length 3})])))

(deftest test-focus-query-union
  (is (= (om/focus-query [{:selected/item {:item/one [:title]
                                           :item/two [:author]}}]
                         [:selected/item :item/two])
         [{:selected/item {:item/two [:author]}}])))

(deftest test-om-466
  (is (= (om/focus-query
           '[:foo/bar ({:dashboard/posts [:id :favorites]} {:foo "bar"})]
           [:dashboard/posts])
         '[({:dashboard/posts [:id :favorites]} {:foo "bar"})])))

(deftest test-om-656
  (is (= (-> (om/focus-query [^:query-root {:child [:foo :bar]}] [:child])
           first meta) {:query-root true}))
  (is (= (-> (om/focus-query [{:child [^:query-root {:foo [:qux]} :bar]}] [:child])
           first :child first meta) {:query-root true}))
  (is (= (-> (om/focus-query [{:child [^:query-root {:foo [:qux]} :bar]}] [:child :foo])
           first :child first meta) {:query-root true})))

(deftest test-om-615
  (let [q1 '[^:query-root (:test {:a ?a})]
        q2 '[^:query-root {:foo [(:bar {:a ?a}) :b]} (:test {:a ?a, :b ?b})]
        params {:a 1 :b 2}
        bq1 (#'om/bind-query q1 params)
        bq2 (#'om/bind-query q2 params)]
    (is (= bq1 '[(:test {:a 1})]))
    (is (not (nil? (-> bq1 first meta))))
    (is (= bq2 '[{:foo [(:bar {:a 1}) :b]} (:test {:a 1, :b 2})]))
    (is (not (nil? (-> bq2 first meta))))))

(deftest test-om-727
  (let [data {:item [:bar 0]
              :bar    {0 {:id 0 :next [:bar 1]}
                             1 {:id 1 :next [:bar 2]}
                             2 {:id 2}}}]
    (is (= (om/db->tree [{:item {:foo [:id] :bar [:id {:next '...}]}}] data data)
           {:item {:id 0, :next {:id 1, :next {:id 2}}}}))))

(defui ^:private Private
  static om/IQuery
  (query [this]
    [:foo]))

(defui ^:private ^:once PrivateOnce
  static om/IQuery
  (query [this]
    [:foo]))

(deftest test-om-739
  (is (true? (-> #'Private meta :private)))
  (is (true? (-> #'PrivateOnce meta :private)))
  (is (true? (-> #'PrivateOnce meta :once))))

(defui OM-746-Component
  "Some docstring"
  static om/IQuery
  (query [this]
    [:foo]))

(deftest test-om-746
  (is (= (-> #'OM-746-Component meta :doc) "Some docstring")))

(defui OM-773-Child
  static om/IQuery
  (query [this]
    [:foo]))

(defui OM-773-LinkRoot
  static om/IQuery
  (query [this]
    [{[:a '_] (om/get-query OM-773-Child)}]))

(defui OM-773-IdentRoot
  static om/IQuery
  (query [this]
    [{[:users/by-id 2] (om/get-query OM-773-Child)}]))

(deftest test-om-773
  (testing "OM-773: `full-query` thinks no queries exist when the query path includes a link"
    (let [r (om/reconciler
              {:state (atom nil)
               :parser (om/parser {:read (fn [_ _ _] {})})})
          idxr (get-in r [:config :indexer])
          _ (p/add-root! r OM-773-LinkRoot nil nil)
          _ (p/index-component! idxr #?(:clj (OM-773-LinkRoot nil nil #js {:omcljs$reconciler r} nil)
                                        :cljs (OM-773-LinkRoot. #js {:omcljs$reconciler r})))
          root (-> @idxr :class->components (get OM-773-LinkRoot) first)
          child #?(:clj (OM-773-Child nil nil #js {:omcljs$reconciler r
                                                   :omcljs$path [:a]
                                                   :omcljs$parent root} nil)
                   :cljs (OM-773-Child. #js {:omcljs$reconciler r
                                             :omcljs$path [:a]
                                             :omcljs$parent root}))
          _ (p/index-component! idxr child)]
      (is (= (om/full-query child)
            '[{[:a _] [:foo]}])))
    (let [r (om/reconciler
              {:state (atom nil)
               :parser (om/parser {:read (fn [_ _ _] {})})})
          idxr (get-in r [:config :indexer])
          _ (p/add-root! r OM-773-IdentRoot nil nil)
          _ (p/index-component! idxr #?(:clj (OM-773-IdentRoot nil nil #js {:omcljs$reconciler r} nil)
                                        :cljs (OM-773-IdentRoot. #js {:omcljs$reconciler r})))
          root (-> @idxr :class->components (get OM-773-IdentRoot) first)
          child #?(:clj (OM-773-Child nil nil #js {:omcljs$reconciler r
                                                   :omcljs$path [[:users/by-id 2]]
                                                   :omcljs$parent root} nil)
                   :cljs (OM-773-Child. #js {:omcljs$reconciler r
                                             :omcljs$path [[:users/by-id 2]]
                                             :omcljs$parent root}))
          _ (p/index-component! idxr child)]
      (is (= (om/full-query child)
            '[{[:users/by-id 2] [:foo]}])))))

(defprotocol OM-786-Protocol
  (some-func [this]))

(defui OM-786-Component
  static OM-786-Protocol
  (some-func [this]
    42))

(deftest test-om-786
  (is (= #?(:clj  ((-> OM-786-Component meta :some-func) OM-786-Component)
            :cljs (some-func OM-786-Component)) 42)))

(defui OM-802-Child
  static om/Ident
  (ident [this {:keys [type id]}]
    [type id])
  static om/IQuery
  (query [this]
    '[:id :type {:children ...}]))

(defui OM-802-Root
  static om/IQuery
  (query [this]
    [{:root (om/get-query OM-802-Child)}]))

(deftest test-om-802
  (let [state {:root {:id 1 :type :branch
                      :children [{:id 2 :type :leaf}
                                 {:id 3 :type :branch}]}}
        normalized (om/tree->db OM-802-Root state true)]
    (is (contains? (:branch normalized) 3))))

(deftest test-om-808
  (let[p (om/parser {:read (fn [_ _ _] {:value 42})})]
    (is (= (p {} [{:foo [:bar]}])
           {:foo 42}))
    (is (= (p {} [{[:foo/by-id 0] [:bar]}])
           {[:foo/by-id 0] 42}))
    (is (= (p {} [{'[:foo/by-id _] [:bar]}])
           {:foo/by-id 42}))))
;; -----------------------------------------------------------------------------
;; Query Templating

(deftest test-expr->key
  (is (= :foo (#'om/expr->key {:foo []})))
  (is (= :foo (#'om/expr->key '({:foo []} {:bar :baz}))))
  (is (= :foo (#'om/expr->key '[:foo _])))
  (is (= [:foo 0] (#'om/expr->key '[:foo 0]))))

(deftest test-query-template
  (is (= [:app/title {:todos/list [:db/id :todo/title]}]
         (-> (#'om/query-template
               [:app/title {:todos/list [:db/id :todo/title :todo/completed]}]
               [:todos/list])
           (#'om/replace [:db/id :todo/title]))))
  (is (= '[:app/title ({:todos/list [:db/id :todo/title]} {:start 0 :end 10})]
         (-> (#'om/query-template
               '[:app/title ({:todos/list [:db/id :todo/title :todo/completed]}
                             {:start 0 :end 10})]
               '[:todos/list])
           (#'om/replace [:db/id :todo/title])))))

(deftest test-query-template-root
  (is (= [:app/title]
         (-> (#'om/query-template
               [:app/title {:todos/list [:db/id :todo/title :todo/completed]}]
               [])
           (#'om/replace [:app/title])))))

(deftest test-query-template-union
  (is (= (-> (#'om/query-template
               [{:selected/item {:item/one [:title]
                                 :item/two [:author]}}]
               [:selected/item :item/two])
           zip/root)
         [{:selected/item [:author]}])
      (= (-> (#'om/query-template
               [{:selected/item {:item/one [:title]
                                 :item/two [:author]}}]
               [:selected/item :item/two])
           (#'om/replace [:caption]))
        [{:selected/item [:caption]}])))

(deftest test-query-template-params
  (is (= (-> (#'om/query-template
               '[({:join [*]} {:param 42})]
                [:join])
           zip/root)
         '[({:join [*]} {:param 42})]))
  (is (= (-> (#'om/query-template
               '[{:join [(:some/key {:param 42})]}]
                [:join])
           zip/root)
         '[{:join [(:some/key {:param 42})]}]))
  (is (= (-> (#'om/query-template
               '[({:selected/item {:item/one [:title]
                                   :item/two [:author]}}
                  {:some/param 42})]
                [:selected/item :item/two])
           zip/root)
         '[({:selected/item [:author]} {:some/param 42})]))
  (is (= (-> (#'om/query-template
               '[{:selected/item {:item/one [:title]
                                  :item/two [({:author [:name]} {:some/param 42})]}}]
                [:selected/item :item/two])
           zip/root)
         '[{:selected/item [({:author [:name]} {:some/param 42})]}])))

(deftest test-query-template-links-idents
  (is (= (-> (#'om/query-template
               '[{:app/projects [{[:user/by-id 0] [:name]}]}]
               '[:app/projects [:user/by-id 0]])
           zip/root)
         '[{:app/projects [{[:user/by-id 0] [:name]}]}]))
  (is (= (-> (#'om/query-template
               '[{:app/projects [{[:app/current-user _] [:name]}]}]
               '[:app/projects [:app/current-user _]])
           zip/root)
         '[{:app/projects [{[:app/current-user _] [:name]}]}])))

;; -----------------------------------------------------------------------------
;; Indexer

(defui ComponentA
  static om/IQuery
  (query [this]
    '[:foo]))

(defui ComponentB
  static om/IQuery
  (query [this]
    '[:bar]))

(defui RootComponent
  static om/IQueryParams
  (params [this]
    {:component (om/get-query ComponentA)})
  static om/IQuery
  (query [this]
    '[{:components/list ?component}]))

(defui IdxrChild
  static om/IQuery
  (query [_]
    [:name]))

(defui IdxrRoot
  static om/IQuery
  (query [_]
    [{:root [{:child (om/get-query IdxrChild)}]}]))

(defui OM-595-Component
  static om/IQuery
  (query [this]
    '[{:item [:id :title {:next ...}]}]))

(defui IdxrNode
  static om/IQuery
  (query [this]
    '[:node-value {:children ...}]))

(defui IdxrTree
  static om/IQuery
  (query [this]
    [{:tree (om/get-query IdxrNode)}]))

(defui IdxrLinkProp
  static om/IQuery
  (query [this]
    '[:foo [:current-user _]]))

(defui IdxrLinkJoin
  static om/IQuery
  (query [this]
    '[:foo {[:current-user _] [:name :email]}]))

(defui IdxrIdentProp
  static om/IQuery
  (query [this]
    '[:foo [:users/by-id 2]]))

(defui IdxrIdentJoin
  static om/IQuery
  (query [this]
    '[:foo {[:users/by-id 2] [:id :name :email]}]))

(defui IdxrLinkItem
  static om/IQuery
  (query [this]
    [:b]))

(defui IdxrLinkRoot
  static om/IQuery
  (query [this]
    [{[:a '_] (om/get-query IdxrLinkItem)}]))

(defui IdxrParamsComponent
  static om/IQueryParams
  (params [_]
    {:foo ""})
  static om/IQuery
  (query [_]
    '[(:some/key {:foo ?foo})]))

(deftest test-indexer
  (testing "prop->classes"
    (let [idxr (om/indexer)
          idxs (p/index-root idxr ComponentList)]
      (is (= (set (keys (:prop->classes idxs)))
            #{:app/title :components/list :foo/bar :baz/woz})))
    (let [idxr (om/indexer)
          idxs (p/index-root idxr IdxrParamsComponent)]
      (is (= (set (keys (:prop->classes idxs)))
            #{:some/key}))))
  (testing "simple recursion indexing"
    (let [idxr (om/indexer)
          idxs (p/index-root idxr IdxrTree)
          cps (keys (:class-path->query idxs))]
      (is (= (count cps) 2))
      (is (not (nil? (some #{[IdxrTree IdxrNode]} cps))))))
  (testing "OM-595: recursion queries without own component"
    (let [idxr (om/indexer)
          idxs (p/index-root idxr OM-595-Component)
          cps (keys (:class-path->query idxs))]
      (is (= (count cps) 1))
      (is (= (first cps) [OM-595-Component]))))
  (testing "OM-612: regression introduced by OM-595"
    (let [idxr (om/indexer)
          idxs (p/index-root idxr IdxrRoot)
          cps (keys (:class-path->query idxs))]
      (is (not (nil? (some #{[IdxrRoot IdxrChild]} cps))))))
  (testing "OM-620: link & indent indexing"
    (let [idxr (om/indexer)]
      (are [class res] (= (->> class
                            (p/index-root idxr)
                            :prop->classes keys set)
                         res)
        IdxrLinkProp #{:foo :current-user}
        IdxrLinkJoin #{:foo :current-user :name :email}
        IdxrIdentProp #{:foo [:users/by-id 2]}
        IdxrIdentJoin #{:foo [:users/by-id 2] :id :name :email})))
  (testing "OM-639: index-root fails on links"
    (let [idxr (om/indexer)
          idxs (p/index-root idxr IdxrLinkRoot)]
      (is (contains? idxs :prop->classes)))))

(deftest test-reconciler-has-indexer
  (let [r (om/reconciler
            {:state (atom nil)
             :ui->ref identity})]
    (is (instance? om.next.Indexer (get-in r [:config :indexer])))))

(deftest test-reindex-instances
  (let [r (om/reconciler
            {:state (atom nil)
             :parser (om/parser {:read (fn [_ _ _] {})})})
        idxr (get-in r [:config :indexer])
        ;; simulate mounting
        _ (p/add-root! r RootComponent nil nil)
        _ (p/index-component! idxr #?(:clj  (RootComponent nil nil #js {:omcljs$reconciler r} nil)
                                      :cljs (RootComponent. #js {:omcljs$reconciler r})))
        indexes @(:indexes idxr)
        classes (-> indexes :class->components keys)
        cps (-> indexes :class-path->query keys)
        c (first (get-in indexes [:class->components RootComponent]))]
    (is (= (first classes) RootComponent))
    (is (not (nil? (some #{[RootComponent ComponentA]} cps))))
    ;; will reindex
    (om/set-query! c {:params {:component (om/get-query ComponentB)}})
    (let [indexes @(:indexes idxr)
          cps (-> indexes :class-path->query keys)]
      (is (not (nil? (some #{[RootComponent ComponentB]} cps)))))))

(defui Child
  static om/IQueryParams
  (params [this]
    {:count 3})
  static om/IQuery
  (query [this]
    '[:id :title (:items {:count ?count})]))

(defui ToOneRoot
  static om/IQuery
  (query [this]
    [{:tab1 (om/get-query Child)}
     {:tab2 (om/get-query Child)}]))

(defui ToManyRoot
  static om/IQuery
  (query [this]
    [{:children (om/get-query Child)}]))

(defui UnionChildA
  static om/IQuery
  (query [this]
    [:foo {:nested/join (om/get-query Child)}]))

(defui UnionChildB
  static om/IQuery
  (query [this]
    [:bar]))

(defui UnionRoot
  static om/IQuery
  (query [this]
    [{:union {:foo (om/get-query UnionChildA)
              :bar (om/get-query UnionChildB)}}]))

(deftest test-indexer-runtime-tree
  (testing "1->1 joins of the same class in different data paths"
    (let [r (om/reconciler
              {:state (atom nil)
               :parser (om/parser {:read (fn [_ _ _] {})})})
          idxr (get-in r [:config :indexer])
          ;; simulate mounting
          _ (p/add-root! r ToOneRoot nil nil)
          _ (p/index-component! idxr #?(:clj  (ToOneRoot nil nil #js {:omcljs$reconciler r
                                                                     :omcljs$path []} nil)
                                        :cljs (ToOneRoot. #js {:omcljs$reconciler r
                                                               :omcljs$path []})))
          root (-> @idxr :class->components (get ToOneRoot) first)
          _ (p/index-component! idxr #?(:clj  (Child nil nil #js {:omcljs$reconciler r
                                                                  :omcljs$path [:tab1]
                                                                  :omcljs$parent root} nil)
                                        :cljs (Child. #js {:omcljs$reconciler r
                                                           :omcljs$path [:tab1]
                                                           :omcljs$parent root})))
          c (first (get-in @idxr [:data-path->components [:tab1]]))]
      (is (not (nil? c)))
      ;; will reindex
      (om/set-query! c {:query [:foo :bar]})
      (is (= (om/get-query c)
            [:foo :bar]))
      (is (= (om/get-query root)
            '[{:tab1 [:foo :bar]} {:tab2 [:id :title (:items {:count 3})]}]))))
  (testing "children in 1->many joins changing queries should not be reflected at the root"
    (let [r (om/reconciler
              {:state (atom nil)
               :parser (om/parser {:read (fn [_ _ _] {})})})
          idxr (get-in r [:config :indexer])
          _ (p/add-root! r ToManyRoot nil nil)
          _ (p/index-component! idxr #?(:clj  (ToManyRoot nil nil #js {:omcljs$reconciler r
                                                                       :omcljs$path []} nil)
                                        :cljs (ToManyRoot. #js {:omcljs$reconciler r
                                                                :omcljs$path []})))
          root (-> @idxr :class->components (get ToManyRoot) first)
          children (map
                     (fn [idx]
                       #?(:clj  (Child nil nil #js {:omcljs$reconciler r
                                                    :omcljs$path [:children idx]
                                                    :omcljs$parent root} nil)
                          :cljs (Child. #js {:omcljs$reconciler r
                                             :omcljs$path [:children idx]
                                             :omcljs$parent root})))
                     (range 3))
          _ (run! #(p/index-component! idxr %) children)
          c (first (get-in @idxr [:data-path->components [:children]]))]
      (is (not (nil? c)))
      (om/set-query! c {:query [:foo :bar]})
      (is (= (om/get-query c)
            [:foo :bar]))
      (is (= (om/get-query root)
            '[{:children [:id :title (:items {:count 3})]}]))))
  (testing "1->1 union queries can be changed from below"
    (let [r (om/reconciler
              {:state (atom nil)
               :parser (om/parser {:read (fn [_ _ _] {})})})
          idxr (get-in r [:config :indexer])
          _ (p/add-root! r UnionRoot nil nil)
          _ (p/index-component! idxr #?(:clj  (UnionRoot nil nil #js {:omcljs$reconciler r
                                                                      :omcljs$path []} nil)
                                        :cljs (UnionRoot. #js {:omcljs$reconciler r
                                                               :omcljs$path []})))
          root (-> @idxr :class->components (get UnionRoot) first)
          child-b #?(:clj  (UnionChildB nil nil #js {:omcljs$reconciler r
                                                     :omcljs$path [:union]
                                                     :omcljs$parent root} nil)
                     :cljs (UnionChildB. #js {:omcljs$reconciler r
                                              :omcljs$path [:union]
                                              :omcljs$parent root}))
          _ (p/index-component! idxr child-b)
          c (first (get-in @idxr [:data-path->components [:union]]))]
      (is (not (nil? c)))
      (om/set-query! c {:query [:baz :qux]})
      (is (= (om/get-query c)
            [:baz :qux]))
      (is (= (om/get-query root)
            [{:union {:foo (om/get-query UnionChildA)
                      :bar [:baz :qux]}}]))
      ;; changing nested joins in the union
      (let [child-a #?(:clj  (UnionChildA nil nil #js {:omcljs$reconciler r
                                                       :omcljs$path [:union]
                                                       :omcljs$parent root} nil)
                       :cljs (UnionChildA. #js {:omcljs$reconciler r
                                                :omcljs$path [:union]
                                                :omcljs$parent root}))
            child-a-child #?(:clj  (Child nil nil #js {:omcljs$reconciler r
                                                       :omcljs$path [:union :nested/join]
                                                       :omcljs$parent child-a} nil)
                             :cljs (Child. #js {:omcljs$reconciler r
                                                :omcljs$path [:union :nested/join]
                                                :omcljs$parent child-a}))
            _ (p/drop-component! idxr c)
            _ (p/index-component! idxr child-a)
            _ (p/index-component! idxr child-a-child)
            c' (first (get-in @idxr [:data-path->components [:union]]))
            c-child' (first (get-in @idxr [:data-path->components [:union :nested/join]]))]
        (is (= c' child-a))
        (is (not (nil? c-child')))
        (om/set-query! c-child' {:query [:other/key]})
        (is (= (om/get-query root)
              [{:union {:foo [:foo {:nested/join [:other/key]}]
                        :bar [:baz :qux]}}])))))
  (testing "query changes in 1->many unions should not be reflected at the root"
    (let [r (om/reconciler
              {:state (atom nil)
               :parser (om/parser {:read (fn [_ _ _] {})})})
          idxr (get-in r [:config :indexer])
          _ (p/add-root! r UnionRoot nil nil)
          _ (p/index-component! idxr #?(:clj (UnionRoot nil nil #js {:omcljs$reconciler r
                                                                     :omcljs$path []} nil)
                                        :cljs (UnionRoot. #js {:omcljs$reconciler r
                                                               :omcljs$path []})))
          root (-> @idxr :class->components (get UnionRoot) first)
          children (map
                     (fn [idx]
                       #?(:clj  (UnionChildB nil nil #js {:omcljs$reconciler r
                                                          :omcljs$path [:union idx]
                                                          :omcljs$parent root} nil)
                          :cljs (UnionChildB. #js {:omcljs$reconciler r
                                                   :omcljs$path [:union idx]
                                                   :omcljs$parent root})))
                     (range 3))
          _ (run! #(p/index-component! idxr %) children)
          c (first (get-in @idxr [:data-path->components [:union]]))]
      (is (not (nil? c)))
      (om/set-query! c {:query [:baz :qux]})
      (is (= (om/get-query c)
            [:baz :qux]))
      (is (= (om/get-query root)
            [{:union {:foo (om/get-query UnionChildA)
                      :bar (om/get-query UnionChildB)}}])))))

;; -----------------------------------------------------------------------------
;; Parser

(deftest test-expr->ast
  (is (= (parser/expr->ast :foo)
         {:type :prop :key :foo :dispatch-key :foo}))
  (is (= (parser/expr->ast [:foo 0])
         {:type :prop :key [:foo 0] :dispatch-key :foo}))
  (is (= (dissoc (parser/expr->ast {:foo [:bar]}) :children)
         {:type :join, :dispatch-key :foo, :key :foo, :query [:bar]}))
  (is (= (dissoc (parser/expr->ast {[:foo 0] [:bar]}) :children)
         {:type :join, :dispatch-key :foo, :key [:foo 0], :query [:bar]}))
  (is (= (parser/expr->ast '(:foo {:bar 1}))
         {:type :prop :key :foo :dispatch-key :foo :params {:bar 1}}))
  (is (= (dissoc (parser/expr->ast '({:foo [:bar :baz]} {:woz 1})) :children)
         {:type :join, :dispatch-key :foo, :key :foo, :query [:bar :baz]
          :params {:woz 1}}))
  (is (= (dissoc (parser/expr->ast '({[:foo 0] [:bar :baz]} {:woz 1})) :children)
         {:type :join, :dispatch-key :foo, :key [:foo 0], :query [:bar :baz]
          :params {:woz 1}}))
  (is (= (parser/expr->ast '(do/it {:woz 1}))
         {:type :call :key 'do/it :dispatch-key 'do/it :params {:woz 1}}))
  (is (= (parser/expr->ast '(do/it))
         {:type :call :key 'do/it :dispatch-key 'do/it :params {}})))

(deftest test-ast->expr
  (is (= (parser/ast->expr {:type :prop :key :foo :dispatch-key :foo})
         :foo))
  (is (= (parser/ast->expr {:type :prop :key [:foo 0] :dispatch-key :foo})
         [:foo 0]))
  (is (= (parser/ast->expr {:type :join :key :foo :dispatch-key :foo :query [:bar]})
         {:foo [:bar]}))
  (is (= (parser/ast->expr {:type :join :key [:foo 0] :dispatch-key :foo :query [:bar]})
         {[:foo 0] [:bar]}))
  (is (= (parser/ast->expr {:type :prop :key :foo :dispatch-key :foo :params {:bar 1}})
         '(:foo {:bar 1})))
  (is (= (parser/ast->expr {:type :join :key :foo :dispatch-key :foo :query [:bar :baz] :params {:woz 1}})
         '({:foo [:bar :baz]} {:woz 1})))
  (is (= (parser/ast->expr {:type :join :key [:foo 0] :dispatch-key :foo :query [:bar :baz] :params {:woz 1}})
         '({[:foo 0] [:bar :baz]} {:woz 1})))
  (is (= (parser/ast->expr {:type :call :key 'do/it :dispatch-key 'do/it :params {:woz 1}})
         '(do/it {:woz 1})))
  (is (= (parser/ast->expr {:type :call :key 'do/it :dispatch-key 'do/it :params {}})
         '(do/it))))

(deftest test-path-meta
  (testing "joins"
    (let [x {:children [{:name "John" :age 3} {:name "Mary" :age 5}]}
          props (parser/path-meta x [] [{:children [:name :age]}])]
      (is (= props x))
      (are [x path] (= (-> x meta :om-path) path)
        props []
        (:children props) [:children]
        (-> props :children first) [:children 0])))
  (testing "joins & links"
    (let [x {:items [{:id 0 :title "Foo" :current-user {:email "bob.smith@gmail.com"}}
                     {:id 1 :title "Bar" :current-user {:email "bob.smith@gmail.com"}}]}
          props (parser/path-meta x [] '[{:items [:id :title [:current-user _]]}])]
      (is (= props x))
      (are [x path] (= (-> x meta :om-path) path)
        props []
        (:items props) [:items]
        (-> props :items first) [:items 0]
        (-> props :items first :current-user) [:items 0 :current-user]))
    (let [data {:items [{:title "Foo"
                         :current-user {:name "Alice", :email "alice@gmail.com"}}
                        {:title "Bar"
                         :current-user {:name "Alice", :email "alice@gmail.com"}}]}
          query '[{:items [([:current-user _] {:a 42}) :title]}]
          props (parser/path-meta data [] query)]
      (is (= props data))
      (are [x path] (= (-> x meta :om-path) path)
        props []
        (:items props) [:items]
        (-> props :items first) [:items 0]
        (-> props :items first :current-user) [:items 0 :current-user])))
  (testing "unions"
    (let [x {:dashboard/items
             [{:id 0 :type :dashboard/post :author "Laura Smith"
               :title "A Post!" :content "Lorem ipsum dolor" :favorites 0}
              {:id 1 :type :dashboard/photo :title "A Photo!"
               :image "photo.jpg" :caption "Lorem ipsum" :favorites 0}
              {:id 2 :type :dashboard/graphic :title "Charts and Stufff!"
               :image "chart.jpg" :favorites 0}]}
          q [{:dashboard/items
              {:dashboard/post [:id :type :title :author :content :favorites]
               :dashboard/photo [:id :type :title :image :caption :favorites]
               :dashboard/graphic [:id :type :title :image :favorites]}}]
          props (parser/path-meta x [] q)]
      (is (= props x))
      (are [x path] (= (-> x meta :om-path) path)
        props []
        (:dashboard/items props) [:dashboard/items]
        (-> props :dashboard/items first) [:dashboard/items 0]
        (-> props :dashboard/items second) [:dashboard/items 1]))
    ;; test union query inference
    (let [x {:data {:foo {:some/key 42}}}
          q [{:data {:branch/foo [{:foo [:some/key]} :qux]
                     :branch/bar [:bar :baz]}}]
          props (parser/path-meta x [] q)]
      (is (= props x))
      (are [x path] (= (-> x meta :om-path) path)
        props []
        (:data props) [:data]
        (-> props :data :foo) [:data :foo]))
    (let [x {:data {:foo {:some/key 42}
                    :bar 43
                    :baz 44}}
          q [{:data {:branch/one [{:foo [:some/key]} :bar :baz :other]
                     :branch/two [:foo :bar :baz :qux :one/more]
                     :branch/three [{:foo [:some/key]} :bar :baz]}}]
          props (parser/path-meta x [] q)]
      (is (= props x))
      (are [x path] (= (-> x meta :om-path) path)
        props []
        (:data props) [:data]
        (-> props :data :foo) [:data :foo])))
  (testing "recursive queries"
    (let [x {:tree {:node-value 1
                    :children [{:node-value 2
                                :children [{:node-value 3 :children []}]}
                               {:node-value 4 :children []}]}}
          q '[{:tree [:node-value {:children ...}]}]
          props (parser/path-meta x [] q)]
      (is (= props x))
      (are [x path] (= (-> x meta :om-path) path)
        props []
        (:tree props) [:tree]
        (-> props :tree :children) [:tree :children]
        (-> props :tree :children first) [:tree :children 0]
        (-> props :tree :children first :children) [:tree :children 0 :children])))
  (testing "mutations"
    (let [x {'counter/increment
             {:keys []
              :result {:app/title "Hello World!"
                       :app/current-id 3
                       :app/counters {0 {:id 0 :counter/count 1}
                                      1 {:id 1 :counter/count 0}
                                      2 {:id 2 :counter/count 0}}
                       :counters/list [[:app/counters 0] [:app/counters 1] [:app/counters 2]]}}}
          props (parser/path-meta x [] '[(counter/increment)])]
      (is (= props x))
      (is (= (-> props meta :om-path) []))
      (is (= (-> props (get 'counter/increment) meta :om-path) nil))))
  (testing "path-meta doesn't throw on bad input"
    (let [data {:people [[:person/by-id 1]]}
          query [{:people [:person/id :person/name
                           {:roles [:role/id :role/name]}]}]]
      (is (= (parser/path-meta data [] query)
             data)))))

(deftest test-path-meta-lists
  (let [x {:children '({:name "John" :age 3} {:name "Mary" :age 5})}
        props (parser/path-meta x [] [{:children [:name :age]}])]
    (is (= props x))
    (is (= (-> props meta :om-path) []))
    (is (= (-> props :children meta :om-path) [:children]))
    (is (= (-> props :children first meta :om-path) [:children 0])))
  (let [x {:children '({:name {:first "John" :last "Doe"} :age 3} {:name {:first "Mary" :last "Smith"} :age 5})}
        props (parser/path-meta x [] [{:children [{:name [:first :last]} :age]}])]
    (is (= props x))
    (is (= (-> props meta :om-path) []))
    (is (= (-> props :children meta :om-path) [:children]))
    (is (= (-> props :children first :name meta :om-path) [:children 0 :name]))))

(defmulti read (fn [env k params] k))

(defmethod read :default
  [{:keys [state data]} k params]
  (if (and (not (nil? data)) (contains? data k))
    {:value (get data k)}
    {:remote true}))

(defmethod read :foo/bar
  [{:keys [state]} k params]
  (if-let [v (get @state k)]
    {:value v}
    {:remote true}))

(defmethod read :woz/noz
  [{:keys [state]} k params]
  (if-let [v (get @state k)]
    {:value v :remote true} ;; local read AND remote read
    {:remote true})) ;; no cached locally, must read remote

(defmethod read :user/pic
  [env k {:keys [size]}]
  (let [size-str (case size :small "50x50" :large "100x100")]
    {:value (str "user" size-str ".png") :remote true}))

(defmethod read :user/by-id
  [{:keys [query] :as env} k {:keys [id] :as params}]
  {:value (cond-> {:name/first "Bob" :name/last "Smith"}
            query (select-keys query))
   :remote true})

(defmulti mutate (fn [env k params] k))

(defmethod mutate 'do/it!
  [{:keys [state]} k {:keys [id]}]
  {:value {:keys [id]}
   :action (fn [])
   :remote true})

(def p (om/parser {:read read :mutate mutate}))

(deftest test-basic-parsing
  (let [st (atom {:foo/bar 1})]
    (is (= (p {} [:baz/woz]) {}))
    (is (= (p {:state st} [:foo/bar]) {:foo/bar 1}))
    (is (= (p {:state st} [:foo/bar :baz/woz]) {:foo/bar 1}))
    (is (= (p {} [:baz/woz] :remote) [:baz/woz]))
    (is (= (p {:state st} [:foo/bar] :remote) []))
    (is (= (p {:state st} [:foo/bar :baz/woz] :remote) [:baz/woz]))))

(deftest test-value-and-remote
  (let [st (atom {:woz/noz 1})]
    (is (= (p {:state st} [:woz/noz]) {:woz/noz 1}))
    (is (= (p {:state st} [:woz/noz] :remote) [:woz/noz]))))

(deftest test-call
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[(do/it! {:id 0})]) '{do/it! {:keys [0]}}))
    (is (= (p {} '[(do/it! {:id 0})] :remote)
           '[(do/it! {:id 0})]))))

(deftest test-read-call
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[(:user/pic {:size :small})])
           {:user/pic "user50x50.png"}))
    (is (= (p {:state st} '[(:user/pic {:size :small})] :remote)
           '[(:user/pic {:size :small})]))))

(defmethod mutate 'mutate!
  [{:keys [state]} k params]
  {:value  {:keys []}
   :action #(swap! state update-in [:count] inc)} )

(deftest test-remote-does-not-mutate
  (let [st (atom {:count 0})
        _  (p {:state st} '[(mutate!)])
        _  (p (:state st) '[(mutate!)] :remote)]
    (is (= @st {:count 1}))))

(defmethod read :now/wow
  [{:keys [state query]} k params]
  {:value {:query query :params params}})

(deftest test-parameterized-join
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} '[({:now/wow [:a :b]} {:slice [10 20]})])
           '{:now/wow {:query [:a :b] :params {:slice [10 20]}}}))))

(deftest test-refs
  (let [st (atom {:foo/bar 1})]
    (is (= (p {:state st} [[:user/by-id 0]])
           {[:user/by-id 0] {:name/first "Bob" :name/last "Smith"}}))
    (is (= (p {:state st} [[:user/by-id 0]] :remote)
           [[:user/by-id 0]]))
    (is (= (p {:state st} [{[:user/by-id 0] [:name/last]}])
           {[:user/by-id 0] {:name/last "Smith"}}))
    (is (= (p {:state st} [{[:user/by-id 0] [:name/last]}] :remote)
           [{[:user/by-id 0] [:name/last]}]))))

#_(deftest test-forced-remote
  (is (= (p {} '['(foo/bar)]) {}))
  (is (= (p {} '['(foo/bar)] :remote) '[(foo/bar)])))

(defmethod mutate 'this/throws
  [_ _ _]
  {:action #?(:clj  #(throw (Exception.))
              :cljs #(throw (js/Error.)))})

(deftest test-throw
  (is
    (instance? #?(:clj  Exception
                  :cljs js/Error)
      (get-in (p {} '[(this/throws)]) ['this/throws :om.next/error]))))

(defn make-read-fn [ret-atom]
  (fn [{:keys [parser query state] :as env} k _]
    (when (= k :foo)
      (reset! ret-atom (parser env query)))
    {:value (get @state k)}))

(deftest test-recursive-parsing-path-meta
  (let [ret (atom nil)
        p (parser/parser {:read (make-read-fn ret)})
        state {:foo 42 :bar 43}
        r (om/reconciler {:parser p
                          :state state})
        top-level-ret (p (#'om/to-env r) [{:foo [:bar]}])]
    (is (= top-level-ret {:foo 42}))
    (is (= (meta top-level-ret) {:om-path []}))
    (is (= @ret {:bar 43}))
    (is (nil? (meta @ret)))))

;; -----------------------------------------------------------------------------
;; Edge cases

(defmethod read :missing/thing
  [env k params]
  {})

(deftest test-missing-value
  (is (= (p {} [:missing/thing]) {})))

(defmethod mutate 'remote/action
  [env k params] {:remote true})

(defmethod mutate 'action/no-value
  [{:keys [state] :as env} k params]
  {:action (fn [] (reset! state :changed))})

(deftest test-action-no-value
  (let [state (atom nil)]
    (is (= (p {:state state} '[(action/no-value)])
           '{action/no-value {:result :changed}}))
    (is (= :changed @state)))
  (let [state (atom nil)]
    (is (= (p {:state state} '[(action/no-value)] :remote) []))
    (is (= nil @state))))

(defmethod read :app/count
  [{:keys [state]} k _]
  {:value (get @state k)})

(defmethod mutate 'app/inc!
  [{:keys [state]} _ _]
  {:action #(swap! state update-in [:app/count] inc)})

(deftest test-transact!
  (let [r (om/reconciler {:state (atom {:app/count 0})
                          :parser p})
        t-ret (om/transact! r '[(app/inc!)])
        t-err (om/transact! r '[(this/throws)])]
    (is (= (:app/count @r) 1))
    (is (some? t-ret))
    (is (= t-ret {'app/inc! {:result {:app/count 1}}}))
    (is (= (om/transact! r '[(app/inc!) :app/count])
           {'app/inc! {:result {:app/count 2}}
            :app/count 2}))
    (is (some? t-err))
    (is (contains? t-err 'this/throws))
    (is (contains? (get t-err 'this/throws) :om.next/error))
    (is (not (contains? (get t-err 'this/throws) :result)))
    (is (= (get (om/transact! r '[(this/throws) :app/count]) :app/count) 2))))

;; -----------------------------------------------------------------------------
;; Recursive Parsing

(def todos-state
  (atom
    {:todos
     {0 {:id 0
         :title "Walk dog"
         :completed false
         :category 0}
      1 {:id 0
         :title "Get milk"
         :completed true
         :category 0}
      2 {:id 0
         :title "Finish Om Next"
         :completed false
         :category 1}}
     :categories {0 :home 1 :work}
     :todos/list [0 1 2]}))

(defmethod read :category
  [{:keys [state data]} k _]
  {:value (get-in @state [:categories (get data k)])})

(defmethod read :todos/list
  [{:keys [state query parser] :as env} _ _]
  (let [st @state
        pf #(parser (assoc env :data %) query)]
    {:value (into [] (comp (map (:todos st)) (map pf))
              (:todos/list st))}))

(deftest test-recursive-parse
  (is (= (p {:state todos-state} '[{:todos/list [:title :category]}])
         '{:todos/list [{:title "Walk dog", :category :home}
                        {:title "Get milk", :category :home}
                        {:title "Finish Om Next", :category :work}]})))

;; -----------------------------------------------------------------------------
;; Normalization

(def data
  {:list/one [{:name "John" :points 0 :friend {:name "Bob"}}
              {:name "Mary" :points 0 :foo :bar}
              {:name "Bob" :points 0 :friend {:name "John"}}]
   :list/two [{:name "Gwen" :points 0 :friends [{:name "Jeff"}]}
              {:name "Mary" :points 0 :baz :woz}
              {:name "Jeff" :points 0 :friends [{:name "Gwen"}]}]})

(defui Person
  static om/Ident
  (ident [this {:keys [name]}]
    [:person/by-name name])
  static om/IQuery
  (query [this]
    [:name :points
     {:friend (om/tag [:name] Person)}
     {:friends (om/tag [:name] Person)}
     :foo :baz])
  Object
  (render [this]))

(defui ListView
  Object
  (render [this]))

(defui RootView
  static om/IQuery
  (query [this]
    (let [subquery (om/get-query Person)]
      [{:list/one subquery} {:list/two subquery}]))
  Object
  (render [this]))

(deftest test-tree->db
  (let [norm (om/tree->db RootView data)
        refs (meta norm)
        p0   (get-in refs [:person/by-name "Mary"])]
    (is (= 3 (count (get norm :list/one))))
    (is (= {:name "John" :points 0 :friend [:person/by-name "Bob"]}
           (get-in refs [:person/by-name "John"])))
    (is (= 3 (count (get norm :list/two))))
    (is (contains? p0 :foo))
    (is (contains? p0 :baz))))

(deftest test-incremental-tree->db
  (let [p0   (om/tree->db Person
               {:name "Susan" :points 5 :friend {:name "Mary"}})
        refs (meta p0)]
    (is (= {:name "Susan" :points 5 :friend [:person/by-name "Mary"]}
           p0))
    (is (= refs {:person/by-name {"Mary" {:name "Mary"}}}))))

;; -----------------------------------------------------------------------------
;; Denormalization

(deftest test-db->tree
  (let [orig {:name "Susan" :points 5 :friend {:name "Mary"} :friends []}
        p0   (om/tree->db Person orig)
        refs (meta p0)]
    (is (= orig (om/db->tree (om/get-query Person) p0 refs)))))

(def people-data
  {:people [{:id 0 :name "Bob" :friends []}
            {:id 1 :name "Laura" :friends []}
            {:id 2 :name "Mary" :friends []}]})

(defui Friend1
  static om/Ident
  (ident [this props]
    [:person/by-id (:id props)])
  static om/IQuery
  (query [this]
    [:id :name]))

(defui Person1
  static om/Ident
  (ident [this props]
    [:person/by-id (:id props)])
  static om/IQuery
  (query [this]
    [:id :name {:friends (om/get-query Friend1)}]))

(defui People1
  static om/IQuery
  (query [this]
    [{:people (om/get-query Person1)}]))

(defmulti read2 om/dispatch)

(defmethod read2 :people
  [{:keys [state query] :as env} key _]
  (let [st @state]
    {:value (om/db->tree query (get st key) st)}))

(defn add-friend [state id friend]
  (if (not= id friend)
    (letfn [(add* [friends ref]
              (cond-> friends
                (not (some #{ref} friends)) (conj ref)))]
      (-> state
        (update-in [:person/by-id id :friends]
          add* [:person/by-id friend])
        (update-in [:person/by-id friend :friends]
          add* [:person/by-id id])))
    state))

(deftest test-db->tree-param
  (let [data {:people [:by-name "Alice"]
              :by-name {"Alice" {:name "Alice" :age 31}
                        "Bob" {:name "Bob" :age 25}}}
        query '[(:people {:length 3})]]
    (is (= (om/db->tree query data data) {:people [:by-name "Alice"]}))))

(deftest test-db->tree-param-link
  (let [data {:current-user {:name "Alice" :email "alice@gmail.com"}
              :items [[:items/by-id 0] [:items/by-id 1]]
              :items/by-id {0 {:id 0 :title "Foo"}
                            1 {:id 1 :title "Bar"}}}
        simple-query '[{:items [([:current-user _] {:a 3}) :title]}]
        join-query '[{:items [({[:current-user _] [:name]} {:a 3}) :title]}]]
    (is (= (om/db->tree simple-query data data) {:items [{:title "Foo"
                                                          :current-user {:name "Alice"
                                                                         :email "alice@gmail.com"}}
                                                         {:title "Bar"
                                                          :current-user {:name "Alice"
                                                                         :email "alice@gmail.com"}}]}))
    (is (= (om/db->tree join-query data data) {:items [{:title "Foo"
                                                        :current-user {:name "Alice"}}
                                                       {:title "Bar"
                                                        :current-user {:name "Alice"}}]}))))

(deftest test-db->tree-collection
  (let [norm-data  (om/tree->db People1 people-data true)
        app-state  (atom norm-data)
        parser     (om/parser {:read read2})
        norm-data' (add-friend (om/tree->db People1 people-data true) 0 1)
        app-state' (atom norm-data')]
    (is (= (parser {:state app-state} (om/get-query People1))
           {:people [{:id 0, :name "Bob", :friends []}
                     {:id 1, :name "Laura", :friends []}
                     {:id 2, :name "Mary", :friends []}]}))
    (is (= (parser {:state app-state'} (om/get-query People1))
          {:people [{:id 0, :name "Bob", :friends [{:id 1, :name "Laura"}]}
                    {:id 1, :name "Laura", :friends [{:id 0, :name "Bob"}]}
                    {:id 2, :name "Mary", :friends []}]}))))

(deftest db->tree-graph-loops
  (let [sam {:db/id 1 :person/name "Sam" :person/mate [:people/by-id 2]}
        jenny {:db/id 2 :person/name "Jenny" :person/mate [:people/by-id 1]}
        app-state {:widget/people [[:people/by-id 1] [:people/by-id 2]] :people/by-id {1 sam 2 jenny}}]
    (is (= {[:people/by-id 1] {:person/name "Sam",
                               :person/mate {:person/name "Jenny",
                                             :person/mate [:people/by-id 1]}}}
           (om/db->tree '[{[:people/by-id 1] [:person/name {:person/mate ...}]}] app-state app-state)))))

(deftest db->tree-graph-loops-allow-recursion-through-non-singletons
  (let [db {:nodes [[:node "r/ui"] [:node "r/ui/red-button"] [:node "r/ui/red-button/big"]],
            :node {"r/ui/red-button" {:name "r/ui/red-button", :title "There is a red button",
                                      :children [[:node "r/ui/red-button/big"]], :parent [:node "r/ui"]}
                   "r/ui" {:name "r/ui", :title "UI Requirements",
                           :children [[:node "r/ui/red-button"]], :parent nil}
                   "r/ui/red-button/big" {:name "r/ui/red-button/big", :title "The red button is big",
                                          :children [], :parent [:node "r/ui/red-button"]}}}]
    (testing "Tree graph loops allow recursion through non-singletons"
      (is (= {:nodes [{:name "r/ui",
                      :title "UI Requirements",
                      :children [{:name "r/ui/red-button",
                                  :title "There is a red button",
                                  :parent {:name "r/ui", :title "UI Requirements"},
                                  :children [{:name "r/ui/red-button/big",
                                              :title "The red button is big",
                                              :parent {:name "r/ui/red-button", :title "There is a red button"},
                                              :children []}]}]}
                     {:name "r/ui/red-button",
                      :title "There is a red button",
                      :parent {:name "r/ui", :title "UI Requirements"},
                      :children [{:name "r/ui/red-button/big",
                                  :title "The red button is big",
                                  :parent {:name "r/ui/red-button",
                                           :title "There is a red button"},
                                  :children []}]}
                     {:name "r/ui/red-button/big",
                      :title "The red button is big",
                      :parent {:name "r/ui/red-button",
                               :title "There is a red button"},
                      :children []}]}
           (om/db->tree [{:nodes [:name :title {:parent [:name :title]} {:children '...}]}] db db))))
    (testing "Tree graph loops handle simultaneous recursive graph walk"
      (is (=
           {:nodes [{:name "r/ui",
                     :title "UI Requirements",
                     :children [{:name "r/ui/red-button",
                                 :title "There is a red button",
                                 :parent {:name "r/ui", :title "UI Requirements"},
                                 :children [{:name "r/ui/red-button/big",
                                             :title "The red button is big",
                                             :parent {:name "r/ui/red-button",
                                                      :title "There is a red button",
                                                      :parent {:name "r/ui",
                                                               :title "UI Requirements"}},
                                             :children []}]}]}
                    {:name "r/ui/red-button",
                     :title "There is a red button",
                     :parent {:name "r/ui", :title "UI Requirements"},
                     :children [{:name "r/ui/red-button/big",
                                 :title "The red button is big",
                                 :parent {:name "r/ui/red-button",
                                          :title "There is a red button",
                                          :parent {:name "r/ui",
                                                   :title "UI Requirements"}},
                                 :children []}]}
                    {:name "r/ui/red-button/big",
                     :title "The red button is big",
                     :parent {:name "r/ui/red-button",
                              :title "There is a red button",
                              :parent {:name "r/ui", :title "UI Requirements"}},
                     :children []}]}
           (om/db->tree [{:nodes [:name :title {:parent [:name :title {:parent '...}]} {:children '...}]}] db db))))))

(deftest db->tree-unions
  (let [db {:panels        [[:panelA :ui] [:panelB :ui] [:panelC :ui]]
            :current-panel [:panelA :ui]
            :panelA        {:ui {:boo 42}}
            :panelB        {:ui {:goo 8}}
            :panelC        {:ui {:sticky true}}}
        union {:panelC [:sticky], :panelA [:boo], :panelB [:goo]}
        list-query [{:panels union}]
        single-query [{:current-panel union}]
        ident-query [{[:panelC :ui] union}]]
    (is (= {:panels [{:boo 42} {:goo 8} {:sticky true}]} (om/db->tree list-query db db)))
    (is (= {:current-panel {:boo 42}} (om/db->tree single-query db db)))
    (is (= {[:panelC :ui] {:sticky true}} (om/db->tree ident-query db db)))))

(deftest test-db->tree-ident-chain
  (testing "simple joins"
    (let [data {:a [:b 1]
                :b {1 [:c 2]}
                :c {2 {:foo "bar"}}}
          query [{:a [:foo]}]]
      (is (= (om/db->tree query data data)
            {:a {:foo "bar"}}))))
  (testing "simple unions"
    (let [data {:a [:b 1]
                :b {1 [:c 2]}
                :c {2 {:foo "foo" :bar "bar"}}}]
      (is (= (om/db->tree [{:a {:b [:x] :c [:foo]}}] data data)
             {:a {:foo "foo"}}))))
  (testing "unions with idents in queries"
    (let [data {:y {"bla" [:z "meh"]}
                :z {"meh" {:name "Bar" :age 25}}}]
      (is (= (om/db->tree [{[:y "bla"] {:z [:name]}}] data data)
             {[:y "bla"] {:name "Bar"}})))))

(deftest test-recursion-limit-grammar
  (let [original-query [:k {:j 2}]
        ast (om/query->ast original-query)
        child (-> (om/query->ast original-query) :children second)
        query (om/ast->query ast)]
    (is (= 2 (:query child)))
    (is (= original-query query))
    (testing "ast symmetry on infinite recursion"
      (are [q] (= q (om/ast->query (om/query->ast q)))
               [:a {:b '...}]))))

(deftest test-reduce-query-depth
  (is (= [:a :b {:c 2}] (om/reduce-query-depth [:a :b {:c 3}] :c)))
  (is (= [:a {:j 4} :b {:c 2}] (om/reduce-query-depth [:a {:j 4} :b {:c 3}] :c)))
  (is (= [:a {:j 3} :b {:c 3}] (om/reduce-query-depth [:a {:j 4} :b {:c 3}] :j)))
  (testing "recursive queries that should not be affected"
    (are [q k] (= q (om/reduce-query-depth q k))
            [:a :b {:c '...}] :c
            [:a :b {:j '...} {:c {:x [:a] :y [:b]}}] :j))
  (testing "OM-634: recursive union branches might not recurse"
    (is (= (om/reduce-query-depth [:foo :bar] :missing)
           [:foo :bar]))))

(defui BulletItem
  static om/Ident
  (ident [this {:keys [id]}] [:item/by-id id])
  static om/IQuery
  (query [this] [:id :label {:subitems 1}]))

(defui BulletList
  static om/IQuery
  (query [this] [{:items (om/get-query BulletItem)}]))

(def recursion-limit-data {:items        [[:items/by-id 1] [:items/by-id 5]]
                            :items/by-id {1 {:id 1 :label "A" :subitems [:items/by-id 2]}
                                          2 {:id 2 :label "B" :subitems [:items/by-id 3]}
                                          3 {:id 3 :label "C" :subitems [:items/by-id 4]}
                                          4 {:id 4 :label "D"}
                                          5 {:id 5 :label "E"}}})

(deftest test-db->tree-recursion-limit
  (is (= {:items [{:id 1 :label "A" :subitems {:id 2 :label "B"}}
                  {:id 5 :label "E"}]}
         (om/db->tree (om/get-query BulletList) recursion-limit-data recursion-limit-data))))

(deftest test-tree->db-normalizes-in-presence-of-recursion-limit
  (is (= {:items [[:item/by-id 1]],
          :item/by-id {1 {:id 1, :label "A", :subitems [[:item/by-id 2]]},
                       2 {:id 2, :label "B", :subitems [:item/by-id 3]},
                       3 {:id 3, :label "C"}},
          :om.next/tables #{:item/by-id}}
         (om/tree->db (om/get-query BulletList) {:items [{:id 1 :label "A" :subitems [{:id 2 :label "B" :subitems {:id 3 :label "C"}}]}]} true))))

;; -----------------------------------------------------------------------------
;; Message Forwarding

(defui Post
  static om/IQuery
  (query [this]
    [:id :type :title :author :content]))

(defui Photo
  static om/IQuery
  (query [this]
    [:id :type :title :image :caption]))

(defui Graphic
  static om/IQuery
  (query [this]
    [:id :type :title :image]))

(defui DashboardItem
  static om/Ident
  (ident [this {:keys [id type]}]
    [type id])
  static om/IQuery
  (query [this]
    (zipmap
      [:dashboard/post :dashboard/photo :dashboard/graphic]
      (map #(conj % :favorites)
        [(om/get-query Post)
         (om/get-query Photo)
         (om/get-query Graphic)]))))

(defui Dashboard
  static om/IQuery
  (query [this]
    [{:dashboard/items (om/get-query DashboardItem)}]))

(defmulti read1 om/dispatch)

(defmethod read1 :default
  [_ _ _])

(defmethod read1 :dashboard/items
  [{:keys [parse ast] :as env} _ _]
  {:remote (update-in ast [:query]
             #(into {} (map (fn [[k v]] [k [:favorites]])) %))})

(deftest test-recursive-remote
  (let [parser (om/parser {:read read1})]
    (is (= (parser {} (om/get-query Dashboard) :remote)
           [{:dashboard/items
             {:dashboard/post    [:favorites],
              :dashboard/photo   [:favorites],
              :dashboard/graphic [:favorites]}}]))))

;; -----------------------------------------------------------------------------
;; Recursive Components

(def tree-data
  {:tree {:id 0
          :node-value 1
          :children [{:id 1
                      :node-value 2
                      :children [{:id 2
                                  :node-value 3
                                  :children []}]}
                     {:id 3
                      :node-value 4
                      :children []}]}})

(defui Node
  static om/Ident
  (ident [this {:keys [id]}]
    [:node/by-id id])
  static om/IQuery
  (query [this]
    '[:id :node-value {:children ...}]))

(defui Tree
  static om/IQuery
  (query [this]
    [{:tree (om/get-query Node)}]))

(defmulti tree-read om/dispatch)

(defmethod tree-read :default
  [{:keys [data] :as env} k _]
  {:value (get data k)})

(defmethod tree-read :children
  [{:keys [data parser query] :as env} _ _]
  {:value (let [f #(parser (assoc env :data %) query)]
            (into [] (map f (:children data))))})

(defmethod tree-read :tree
  [{:keys [state parser query] :as env} k _]
  (let [st @state]
    {:value (parser (assoc env :data (:tree st)) query)}))

(deftest test-recursion-syntax
  (let [tree-parser (om/parser {:read tree-read})]
    (is (= tree-data
           (tree-parser {:state (atom tree-data)}
             (om/get-query Tree))))))

(deftest test-normalize-recursive
  (let [db (om/tree->db Tree tree-data true)]
    (is (= [:node/by-id 0] (:tree db)))
    (is (contains? db :node/by-id))
    (is (= tree-data
           (om/db->tree (om/get-query Tree)
             (om/tree->db Tree tree-data)
             (om/tree->db Tree tree-data true))))))

(deftest test-normalize-ref
  (let [db   (om/tree->db Tree tree-data true)
        tree (om/db->tree (-> (om/get-query Tree) ffirst second)
               (:tree db) db)]
    (is (= tree (:tree tree-data)))))

;; -----------------------------------------------------------------------------
;; Recursive Unions

(def union-tree-data
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

(defui UnionBarNode
  static om/IQuery
  (query [this]
    '[:id :node/type :bar/value {:children ...}]))

(defui UnionFooNode
  static om/IQuery
  (query [this]
    '[:id :node/type :foo/value {:children ...}]))

(defui ItemNode
  static om/Ident
  (ident [this {:keys [node/type id]}]
    [type id])
  static om/IQuery
  (query [this]
    {:tree/foo (om/get-query UnionFooNode)
     :tree/bar (om/get-query UnionBarNode)}))

(defui UnionTree
  static om/IQuery
  (query [this]
    `[{:tree ~(om/get-query ItemNode)}]))

(deftest test-normalize-recursive-union
  (let [db (om/tree->db UnionTree union-tree-data true)]
    (is (= [:tree/foo 0] (:tree db)))
    (is (contains? db :tree/foo))
    (is (contains? db :tree/bar))
    (is (= union-tree-data
           (om/db->tree (om/get-query UnionTree)
             (om/tree->db UnionTree union-tree-data)
             db)))))

;; -----------------------------------------------------------------------------
;; Recursive Unions + recursion limit

(def union-tree-limit-by-1-result
  {:tree
   {:id 0 :node/type :tree/foo
    :foo/value "1"
    :children
    [{:id 1 :node/type :tree/bar :bar/value "1.1"}
     {:id 3 :node/type :tree/foo :foo/value "1.2"}]}})

(def union-tree-different-limit-data
  {:tree
    {:node/type :tree/qux
     :id 0
     :qux/value "1"
     :children [{:node/type :tree/foo
                 :foo/value "1.1"
                 :id 1
                 :children
                   [{:node/type :tree/foo
                     :id 2
                     :foo/value "1.1.1"
                     :children
                       [{:node/type :tree/foo
                         :id 3
                         :foo/value "1.1.1.1"
                         :children []}]}]}
                {:node/type :tree/qux
                 :id 4
                 :qux/value "1.2"
                 :children
                  [{:node/type :tree/foo
                    :id 5
                    :foo/value "1.2.1"}]}]}})

(def union-tree-different-limit-result
  {:tree
    {:node/type :tree/qux
     :id 0
     :qux/value "1"
     :children [{:node/type :tree/foo
                 :foo/value "1.1"
                 :id 1}
                ;; qux allowed to recurse twice, so children included in result
                {:node/type :tree/qux
                 :id 4
                 :qux/value "1.2"
                 :children
                   [{:node/type :tree/foo
                     :id 5
                     :foo/value "1.2.1"}]}]}})

(defui UnionLimitBarNode
  static om/IQuery
  (query [this]
    '[:id :node/type :bar/value {:children 1}]))

(defui UnionLimitFooNode
  static om/IQuery
  (query [this]
    '[:id :node/type :foo/value {:children 1}]))

(defui ItemSameLimitNode
  static om/Ident
  (ident [this {:keys [node/type id]}]
    [type id])
  static om/IQuery
  (query [this]
    {:tree/foo (om/get-query UnionLimitFooNode)
     :tree/bar (om/get-query UnionLimitBarNode)}))

(defui UnionSameLimitTree
  static om/IQuery
  (query [this]
    `[{:tree ~(om/get-query ItemSameLimitNode)}]))

(defui UnionLimitQuxNode
  static om/IQuery
  (query [this]
    '[:id :node/type :qux/value {:children 2}]))

(defui ItemDifferentLimitNode
  static om/Ident
  (ident [this {:keys [node/type id]}]
    [type id])
  static om/IQuery
  (query [this]
    {:tree/foo (om/get-query UnionLimitFooNode)
     :tree/qux (om/get-query UnionLimitQuxNode)}))

(defui UnionDifferentLimitTree
  static om/IQuery
  (query [this]
    `[{:tree ~(om/get-query ItemDifferentLimitNode)}]))

(deftest test-db->tree-union-recursion-limit
  (testing "same recursion limit in all union branches"
    (let [db (om/tree->db UnionSameLimitTree union-tree-data true)]
      (is (= union-tree-limit-by-1-result
             (om/db->tree (om/get-query UnionSameLimitTree)
               (om/tree->db UnionSameLimitTree union-tree-data)
               db)))))
    (testing "different recursion limit in union branches"
      (let [db (om/tree->db UnionDifferentLimitTree union-tree-different-limit-data true)]
        (is (= union-tree-different-limit-result
               (om/db->tree (om/get-query UnionDifferentLimitTree)
                 (om/tree->db UnionDifferentLimitTree union-tree-different-limit-data)
                 db))))))

;; -----------------------------------------------------------------------------
;; Path Optimization

(defmethod tree-read :node/by-id
  [{:keys [state query query-root]} _ _]
  {:value (om/db->tree query query-root @state)})

(deftest test-read-ident
  (let [state  (atom (om/tree->db Tree tree-data true))
        parser (om/parser {:read tree-read})
        env    {:state state}
        ident  [:node/by-id 1]]
    (is (= (get (parser env [{ident (om/get-query Node)}]) ident)
           {:id 1, :node-value 2, :children [{:id 2, :node-value 3, :children []}]}))))

;; -----------------------------------------------------------------------------
;; tempids

(deftest test-temp-id-equality
  (let [uuid #?(:clj  (java.util.UUID/randomUUID)
                :cljs (random-uuid))
        id0  (tempid uuid)
        id1  (tempid uuid)]
    (is (= id0 id1))
    (is (= (hash id0) (hash id1)))))

(deftest test-tempid-migration
  (let [db  (assoc-in (om/tree->db Tree tree-data true)
              [:node/by-id 6] {:id 6})
        db' (om/default-migrate db (om/get-query Tree)
              {[:node/by-id 2] [:node/by-id 6]} :id)]
    (is (nil? (get-in db' [:node/by-id 2])))
    (is (= (dissoc (get-in db  [:node/by-id 2]) :id)
           (dissoc (get-in db' [:node/by-id 6]) :id)))))

(def tid (om/tempid))

(def temp-tree-data
  {:tree {:id 0
          :node-value 1
          :children [{:id 1
                      :node-value 2
                      :children [{:id 2
                                  :node-value 3
                                  :children []}]}
                     {:id tid
                      :node-value 4
                      :children []}]}})

(deftest test-real-tempid-migration
  (let [db  (assoc-in (om/tree->db Tree tree-data true)
              [:node/by-id 6] {:id 6})
        db' (om/default-migrate db (om/get-query Tree)
              {[:node/by-id tid] [:node/by-id 6]} :id)]
    (is (nil? (get-in db' [:node/by-id tid])))
    (is (= (dissoc (get-in db  [:node/by-id tid]) :id)
           (dissoc (get-in db' [:node/by-id 6]) :id)))))

;; -----------------------------------------------------------------------------
;; Precise Remoting

(defmulti precise-read om/dispatch)

(defmethod precise-read :default
  [_ _ _]
  {:value :not-found})

(defmethod precise-read :fake/key
  [{:keys [parser ast] :as env} _ _]
  {:remote (update-in ast [:query]
             #(parser env % :remote))})

(defmethod precise-read :real/key
  [{:keys [ast] :as env} _ _]
  {:remote (assoc ast :query-root true)})

(defmethod precise-read :other/key
  [{:keys [ast] :as env} _ _]
  {:remote ast})

(defn precise-mutate [{:keys [ast] :as env} _ _]
  {:remote true })

(deftest test-query-root
  (let [ast (assoc (parser/expr->ast {:real/key [:id]})
              :query-root true)]
    (is (= (meta (parser/ast->expr ast)) {:query-root true}))))

; process-roots can cause duplicate top-level queries. merge-joins is used to pull them together
(deftest test-merge-joins-on-non-merges
  (are [merged raw] (= merged (#'om/merge-joins raw))
    ; calls
    '[(app/f) :a (app/g)] '[(app/f) :a (app/g)]
    ; plain properties
    '[:a :b] '[:a :b]
    ; ident as key
    [{[:db/id 1] [:a :b]}] [{[:db/id 1] [:a :b]}]
    ; unions
    [{:join {:a [:type] :b [:type]}}] [{:join {:a [:type] :b [:type]}}]
    [{:widget {:a [:type] :b [:type]}} {:j [:a]}] [{:j [:a]} {:widget {:a [:type] :b [:type]}}]))

(deftest test-merge-joins-eliminates-exact-duplicates
  (are [merged raw] (= merged (#'om/merge-joins raw))
    [:a] [:a :a]
    [{:j1 [:a :b]}] [{:j1 [:a :b]} {:j1 [:a :b]}]))

(deftest test-merge-joins-merges-simple-joins
  (is (= [{:j [:a :b]}] (#'om/merge-joins [{:j [:a]} {:j [:b]}]))))

(deftest test-merge-joins-eliminates-duplicate-selector-elements
  (is (= [{:j [:a :b]}] (#'om/merge-joins [{:j [:a]} {:j [:a :b]}]))))

(deftest test-merge-joins-merges-nested-joins-of-duplicates
  (is (= [{:j [:b {:a [:x :y]}]}] (#'om/merge-joins [{:j [{:a [:x]}]} {:j [{:a [:y]} :b]}]))))

(deftest test-merge-joins-retains-property-and-call-order-at-top-level
  (is (= '[(app/f) :b :d :e {:j [:b {:a [:x :y]}]} {:k [:x {:y [:b]}]} {:c [:ca :cb]} {:l [:m]}]
          (#'om/merge-joins '[(app/f) {:j [{:a [:x]}]} :b {:k [:x]}
                              {:c [:ca :cb]} {:j [{:a [:y]} :b]} :d
                              {:k [{:y [:b]}]} :e {:l [:m]}]))))

(deftest test-merge-joins-handles-recursive-queries
  (is (= [{:j '...}] (#'om/merge-joins [{:j '...} {:j '...}]))))

(deftest test-process-roots-recursive
  (let [p (om/parser {:read precise-read})
        m (om/process-roots
            (p {:state (atom {})}
              '[{:fake/key [{:real/key ...}]}] :remote))]
    (is (= [{:real/key '...}] (:query m)))))

(deftest test-process-roots-recursive-non-query-root
  (let [p (om/parser {:read precise-read})
        m (om/process-roots
            (p {:state (atom {})}
              '[{:fake/key [{:real/key ...}]} {:other/key [:other/key {:other/key ...}]}] :remote))]
    (is (= [{:real/key '...} {:other/key [:other/key {:other/key '...}]}] (:query m)))))

(deftest test-process-roots-keeps-top-rooted-keys
  (let [p (om/parser {:read precise-read :mutate precise-mutate})
        m (om/process-roots
            (p {:state (atom {})}
               '[:other/key {:fake/key [{:real/key ...}]} {:other/key [:x]}] :remote))]
    (is (= '[:other/key {:real/key ...} {:other/key [:x]}] (:query m)))))

(deftest test-process-roots-keeps-mutations
  (let [p (om/parser {:read precise-read :mutate precise-mutate})
        m (om/process-roots
            (p {:state (atom {})}
               '[(app/f) {:fake/key [{:real/key ...}]} (app/g)] :remote))]
    (is (= '[(app/f) (app/g) {:real/key ...}] (:query m)))))

(deftest test-process-roots-promotes-non-sub-rooted-queries
  (let [ref-rooted-join (with-meta {[:db/id 4] [:a :b]} {:query-root true})
        j1 (with-meta {:j1 [:a :b]} {:query-root true})
        sub-rooted-join (with-meta {:subrooted-join [:x j1]} {:query-root true})]
    (let [{:keys [query rewrite]} (om/process-roots [{:top [ref-rooted-join j1]}])]
      (testing "Process roots promots non-sub-rooted queries"
        (is (= [{[:db/id 4] [:a :b]} {:j1 [:a :b]}] query)))
      (testing "Rewrite result for promoted non-sub-rooted queries"
        (let [top-result {[:db/id 4] {:a 4 :b 5} :j1 {:a 9 :b 10}}
              expected-rewritten-result {:top {[:db/id 4] {:a 4 :b 5}
                                               :j1 {:a 9 :b 10}}}]
          (is (= expected-rewritten-result (rewrite top-result))))))
    (let [{:keys [query rewrite]} (om/process-roots [{:top [j1 sub-rooted-join]}])]
      (testing "Process roots ignore subroots"
        (is (= [{:j1 [:a :b]} {:subrooted-join [:x {:j1 [:a :b]}]}] query)))
      (testing "Rewrite roots on ignored subroots"
        (let [top-result {:j1 {:a 9 :b 10} :subrooted-join {:x 11 :j1 {:a 9 :b 10}}}
              expected-rewritten-result {:top {:j1 {:a 9 :b 10}
                                               :subrooted-join {:x 11 :j1 {:a 9 :b 10}}}}]
          (is (= expected-rewritten-result (rewrite top-result))))))
    (let [top-result {:j1 {:a 9 :b 10}}
          expected-rewritten-result '{:top {:j1 {:a 9 :b 10} :j3 {:j1 {:a 9 :b 10}}}}
          {:keys [query rewrite]} (om/process-roots [{:top [j1 {:j3 [j1]}]}])]
      (testing "Process roots can reroot same join from multiple paths"
        (is (= [{:j1 [:a :b]}] query)))
      (testing "Rewrite of the same join to multiple paths"
        (is (= expected-rewritten-result (rewrite top-result)))))))

(deftest test-process-roots-can-promote-a-union-to-root
  (let [server-query [(with-meta {:dashboard {:photo   [:url]
                                              :comment [:text]}} {:query-root true})]
        full-query [{:ui-key server-query}]
        response {:dashboard {:url "http://images.com/x.gif"}}
        full-response {:ui-key response}
        {:keys [rewrite query]} (om/process-roots full-query)
        ]
    (is (= server-query query))
    (is (= full-response (rewrite response)))))

(deftest test-process-roots-preserves-query-parameters
  (let [re-rooted-query (with-meta '({:dashboard {:photo   [:url]
                                                  :comment [:text]}} {:x 1}) {:query-root true})
        full-query [:a :b {:ui-key [re-rooted-query]}]
        response {:dashboard {:url "http://images.com/x.gif"}}
        full-response {:ui-key response}
        {:keys [rewrite query]} (om/process-roots full-query)]
    (is (= [:a :b re-rooted-query] query))
    (is (= full-response (rewrite response)))))

(deftest test-process-roots-preserves-parameters-2
  (let [parameters  {:user/email "email", :user/password "password"}
        fragment    (with-meta (list {:auth [:token ]}
                                      parameters)
                    {:query-root true})
        full-query [{:current-login-q [fragment]}]
        {:keys [rewrite query]} (om/process-roots full-query)]
    (is (= [fragment] query))))

;; -----------------------------------------------------------------------------
;; User Bugs

(def remove-tree-data
  {:tree {:id 0
          :node-value 1
          :children [{:id 1
                      :random-key :cool!
                      :node-value 2
                      :children [{:id 2
                                  :node-value 3
                                  :children []}]}
                     {:id 3
                      :node-value 4
                      :children []}]}})

(deftest test-db->tree-does-not-remove-keys
  (let [db (om/tree->db Tree remove-tree-data true)]
    (is (= (get-in db [:node/by-id 1 :random-key]) :cool!))))

(defui Page
  static om/Ident
  (ident [this {:keys [id]}]
    [:pages id])
  static om/IQuery
  (query [this]
    [:id :name]))

(def partial-norm-data
  {:pages {0 {:id 0 :name "Foo"}
           1 {:id 1 :name "bar"}}
   :current-page [:pages 0]})

(deftest test-partially-normalized-data
  (is (= (om/tree->db (om/get-query Page) partial-norm-data)
         partial-norm-data)))

(defui SomePost
  static om/Ident
  (ident [this {:keys [id]}]
    [:post/by-id id])
  static om/IQuery
  (query [this]
    '[:id {:user [:username]} :content]))

(defui SomeTimelineComponent
  static om/IQuery
  (query [this]
    (let [subquery (om/get-query SomePost)]
      `[{:app/posts ~subquery}])))

(def posts-data
  {:app/posts [{:user {:username "Bob Smith"}
                :content "Hello World!"
                :id 1}]})

(deftest test-normalize-nested-query-no-class
  (is (= (om/tree->db SomeTimelineComponent posts-data true)
         {:app/posts [[:post/by-id 1]],
          :post/by-id {1 {:user {:username "Bob Smith"},
                          :content "Hello World!", :id 1}},
          :om.next/tables #{:post/by-id}})))

(deftest test-om-637
  (let [data {:some/list [[:item/by-id 1]]
              :item/by-id {1 {:id 1
                              :name "foo"}}}]
    (is (= (om/db->tree '[{:some/list [*]}] data data)
           {:some/list [{:id 1, :name "foo"}]})))
  (is (= (om/db->tree '[[:my-route _]] {:my-route [:some-route]} {:my-route [:some-route]})
         {:my-route [:some-route]}))
  (is (= (om/db->tree '[[:my-route _]] {:my-route [:some-route :other-key]} {:my-route [:some-route :other-key]})
         {:my-route [:some-route :other-key]}))
  (let [data {:todos/list [[:todo/by-id 42]]
              :todo/by-id {42 {:id 42
                               :states [:archived]}}}]
    (is (= (om/db->tree [{:todos/list [:id :states]}] data data)
           {:todos/list [{:id 42, :states [:archived]}]})))
  (let [data {:todos/list [[:todo/by-id 42]]
              :todo/by-id {42 {:id 42
                               :states [:done :archived]}}
              :current-users [{:id 0 :name "John"} {:id 1 :name "Mary"}]}]
    (is (= (om/db->tree [{:todos/list [:id :states '[:current-users _]]}] data data)
           {:todos/list [{:id 42, :states [:done :archived]
                          :current-users [{:id 0 :name "John"}
                                          {:id 1, :name "Mary"}]}]})))
  (let [data {:todos/list [{:id 42 :title "do stuff" :states [:done :archived]}
                           {:id 43 :title "buy milk" :states [:done :archived]}]}]
    (is (= (om/db->tree [{:todos/list [:id :title]}] data data)
           {:todos/list [{:id 42 :title "do stuff"}
                         {:id 43 :title "buy milk"}]}))))

(deftest test-om-604
  (let [data {:items [[:by-id 1]]
              :by-id {1 {:id 1
                         :stuff [{:name "something"}]}}}]
    (is (= (om/db->tree [{:items [:id {:stuff [:name]}]}] data data)
           {:items [{:id 1, :stuff [{:name "something"}]}]})))
  (let [data {:todos/list [{:id 42
                            :author {:first-name "John"
                                     :last-name "Smith"}
                            :states [:done :archived]}
                           {:id 43
                            :author {:first-name "Mary"
                                     :last-name "Brown"}
                            :states [:done :archived]}]}]
    (is (= (om/db->tree [{:todos/list [:id {:author [:first-name]}]}] data data)
           {:todos/list [{:id 42 :author {:first-name "John"}}
                         {:id 43 :author {:first-name "Mary"}}]})))
  (let [data {:items [[:by-id 1]]
              :by-id {1 {:id 1
                         :stuff [{:name {:first "John" :last "Smith"}}]}}}]
    (is (= (om/db->tree [{:items [:id {:stuff [{:name [:first]}]}]}] data data)
          {:items [{:id 1, :stuff [{:name {:first "John"}}]}]}))
    (is (= (om/db->tree [{:items [:id {:stuff ['({:name [:first]} {:param 1})]}]}] data data)
          {:items [{:id 1, :stuff [{:name {:first "John"}}]}]}))))

(deftest test-om-732
  (let [state {:curr-view [:main :view]
               :main {:view {:curr-item [[:sub-item/by-id 2]]}}
               :sub-item/by-id {2 {:foo :baz :sub-items [[:sub-item/by-id 4]]}
                                4 {:foo :bar}}}]
    (is (= (om/db->tree [{:curr-view
                          {:main [{:curr-item [:foo {:sub-items '...}]}]}}] state state)
          {:curr-view {:curr-item [{:foo :baz :sub-items [{:foo :bar}]}]}}))))

(defui IdxrIdentChild
  static om/Ident
  (ident [this params]
    [:users/by-id 2])
  static om/IQuery
  (query [this]
    [:id :name :email]))

(defui IdxrIdentParent
  static om/IQuery
  (query [this]
    [{[:users/by-id 2] (om/get-query IdxrIdentChild)}]))

(deftest test-om-772
  (let [idxr (om/indexer)
        c #?(:clj  (IdxrIdentProp nil nil {:omcljs$path []} nil)
             :cljs (IdxrIdentProp. #js {:omcljs$path []}))]
    (p/index-root idxr IdxrIdentProp)
    (p/index-component! idxr c)
    (is (= (-> @idxr :prop->classes keys set)
           #{:foo [:users/by-id 2]}))
    (is (= (map om/react-type (p/key->components idxr :foo)) [IdxrIdentProp]))
    (is (= (map om/react-type (p/key->components idxr [:users/by-id 2])) [IdxrIdentProp])))
  (let [idxr (om/indexer)
        c #?(:clj  (IdxrLinkProp nil nil {:omcljs$path []} nil)
             :cljs (IdxrLinkProp. #js {:omcljs$path []}))]
    (p/index-root idxr IdxrLinkProp)
    (p/index-component! idxr c)
    (is (= (-> @idxr :prop->classes keys set)
           #{:foo :current-user}))
    (is (= (map om/react-type (p/key->components idxr :foo)) [IdxrLinkProp]))
    (is (= (map om/react-type (p/key->components idxr :current-user)) [IdxrLinkProp]))
    (is (empty? (p/key->components idxr '[:current-user _]))))
  (let [idxr (om/indexer)
        query-c #?(:clj (IdxrIdentParent nil nil {:omcljs$path []} nil)
                   :cljs (IdxrIdentParent. #js {:omcljs$path []}))
        ident-c #?(:clj (IdxrIdentChild nil nil {:omcljs$path [[:users/by-id 2]]} nil)
                   :cljs (IdxrIdentChild. #js {:omcljs$path [[:users/by-id 2]]}))]
    (p/index-root idxr IdxrIdentParent)
    (p/index-component! idxr query-c)
    (p/index-component! idxr ident-c)
    (is (= (set (map om/react-type (p/key->components idxr [:users/by-id 2])))
           #{IdxrIdentParent IdxrIdentChild}))))

;; -----------------------------------------------------------------------------
;; Union Migration

(def union-init-data
  {:dashboard/items
   [{:id 0
     :type :post
     :title "Ohai"}]})

(defui UnionPost
  static om/IQuery
  (query [this]
    [:id :type :title]))

(defui UnionDashboardItem
  static om/Ident
  (ident [this {:keys [id type]}]
    [type id])

  static om/IQuery
  (query [this]
    {:post (om/get-query UnionPost)}))

(defui UnionDashboard
  static om/IQuery
  (query [this]
    [{:dashboard/items (om/get-query UnionDashboardItem)}]))

(defmulti union-read om/dispatch)

(defmethod union-read :dashboard/items
  [{:keys [state query]} k _]
  (let [st @state]
    {:value (om/db->tree query (get-in st k) st)}))

(defmulti union-mutate om/dispatch)

(defmethod union-mutate 'item/add
  [{:keys [state]} k {:keys [id type] :as params}]
  (let [ident [type id]]
    {:action
     (fn []
       (swap! state
         (fn [st]
           (-> st
             (assoc-in ident params)
             (update :dashboard/items conj ident)))))}))

(deftest test-union-migrate
  (let [st (atom (om/tree->db UnionDashboard union-init-data true))
        parser (om/parser {:read union-read :mutate union-mutate})]
    (parser {:state st} '[(item/add {:id -1 :type :post :title "New Stuff!"})])
    (let [db @st
          db' (om/default-migrate db (om/get-query UnionDashboard)
                {[:post -1] [:post 1]} :id)]
      (is (contains? (get db' :post) 1))
      (is (= (get db' :dashboard/items)
             [[:post 0] [:post 1]])))))

;; -----------------------------------------------------------------------------
;; Query Ident Test

(def link-data
  {:current-user {:email "bob.smith@gmail.com"}
   :settings {0 {:foo :bar}}
   :items [{:id 0 :title "Foo"}
           {:id 1 :title "Bar"}
           {:id 2 :title "Baz"}]})

(defmulti link-read om/dispatch)

(defmethod link-read :items
  [{:keys [query state ast]} k _]
  (let [st @state]
    {:value  (om/db->tree query (get st k) st)
     :remote (update-in ast [:query] #(into [] (remove util/ident?) %))}))

(defui LinkItem
  static om/Ident
  (ident [_ {:keys [id]}]
    [:item/by-id id])
  static om/IQuery
  (query [_]
    '[:id :title [:current-user _] [:settings 0]]))

(defui LinkList
  static om/IQuery
  (query [_]
    [{:items (om/get-query LinkItem)}]))

(deftest test-db->tree-idents-in-query
  (let [parser (om/parser {:read link-read})
        state  (atom (om/tree->db LinkList link-data true))
        ui     (parser {:state state} (om/get-query LinkList))]
    (is (every? #(contains? % :current-user) (:items ui)))
    (is (every? #(contains? % [:settings 0]) (:items ui)))
    (is (= (parser {:state state} (om/get-query LinkList) :remote)
           [{:items [:id :title]}]))))

;; -----------------------------------------------------------------------------
;; Query AST API parsing/unparsing

(deftest test-ast<->query
  (let [q0 [{:foo [:bar :baz :woz]}]
        q1 [{:foo [:bar :baz {:woz [:goz :noz]}]}]
        q2 [{:foo {:bar [:baz] :woz [:noz]}}]]
    (is (= q0 (-> q0 om/query->ast om/ast->query)))
    (is (= q1 (-> q1 om/query->ast om/ast->query)))
    (is (= q2 (-> q2 om/query->ast om/ast->query)))))

(defui UiB
  static om/IQuery
  (query [this]
    `[:baz]))

(defui UiA
  static om/IQuery
  (query [this]
    `[:foo {:bar ~(om/get-query UiB)}]))

(defui UiUnion
  static om/IQuery
  (query [_] {:a (om/get-query UiA)
              :b (om/get-query UiB)}))

(deftest test-component-preserved
  (is (= UiA
         (-> (om/get-query UiA) om/query->ast om/ast->query
           meta :component)))
  (is (= UiUnion
         (-> [{:union (om/get-query UiUnion)}] om/query->ast om/ast->query ffirst second
           meta :component))))

;; -----------------------------------------------------------------------------
;; Error handling

(deftest test-unique-ident?
  (is (util/unique-ident? '[:foo _])))

(deftest test-has-error?
  (is (true? (#'om/has-error? (:foo {:foo {::om/error {:type :bar}}})))))

(deftest test-basic-errors
  (let [x (om/default-extract-errors nil
            {:foo {::om/error {:type :oops!}}}
            [:foo])]
    (is (= {:tree nil, :errors {:foo #{{:type :oops!}}}} x)))
  (let [y (om/default-extract-errors nil
            '{:foo {::om/error {:type :oops!}}}
            '[[:foo _]])]
    (is (= {:tree nil, :errors {:foo #{{:type :oops!}}}} y)))
  (let [z (om/default-extract-errors nil
            '{[:foo 0] {::om/error {:type :oops!}}}
            '[[:foo 0]])]
    (is (= {:tree nil, :errors {[:foo 0] #{{:type :oops!}}}} z))))

(defui UiC
  static om/Ident
  (ident [this props]
    [:ui-c (:id props)])
  static om/IQuery
  (query [this]
    [:id :title]))

(deftest test-join-errors
  (let [x (om/default-extract-errors nil
            {:foo [{:id 0 :title "foo"}
                   {:id 1 ::om/error {:type :oops!}}
                   {:id 2 :title "bar"}]}
            [{:foo (om/get-query UiC)}])]
    (is (= {:tree   {:foo [{:id 0, :title "foo"} nil {:id 2, :title "bar"}]},
            :errors {[:ui-c 1] #{{:type :oops!}}}}
           x))))

(defui UiE
  static om/Ident
  (ident [this props]
    [:ui-e (:id props)])
  static om/IQuery
  (query [this]
    [:id :title :author]))

(defui UiD
  static om/Ident
  (ident [this props]
    [:ui-d (:id props)])
  static om/IQuery
  (query [this]
    `[({:foo ~(om/get-query UiE)} {:some-param :some-value})]))

(defui UiC&E
  static om/Ident
  (ident [this {:keys [id type] :as props}]
    [({:c :ui-c :e :ui-e} type) id])
  static om/IQuery
  (query [this]
    {:ui-c (om/get-query UiC)
     :ui-e (om/get-query UiE)}))

(defui UiF
  static om/Ident
  (ident [this props]
    [:ui-f (:id props)])
  static om/IQuery
  (query [this]
    [{:bar (om/get-query UiC&E)}]))

(deftest test-nested-error
  (let [x  (om/default-extract-errors nil
             {:id 0
              :foo {:id 1 :title "Cool"
                    :author "Bob"
                    ::om/error {:type :ouch!}}}
             (om/get-query UiD))
        ys (om/default-extract-errors nil
             {:id 0
              :foo [{:id 1 :title "Cool"
                     :author "Bob"
                     ::om/error {:type :ouch!}}
                    {:id 2 :title "Awesome"
                     :author "George"}]}
             (om/get-query UiD))]
    (is (= {:tree {:foo nil}, :errors {[:ui-e 1] #{{:type :ouch!}}}}
           x))
    (is (= {:tree {:foo [nil {:id 2, :title "Awesome", :author "George"}]}
            :errors {[:ui-e 1] #{{:type :ouch!}}}}
           ys))))

(deftest test-multiple-errors
  (let [x  (om/default-extract-errors nil
             {:id 0
              :foo {:id 1
                    :title {::om/error {:type :ouch!}}
                    :author {::om/error {:type :oof!}}}}
             (om/get-query UiD))
        ys (om/default-extract-errors nil
             {:id 0
              :foo [{:id 1
                     :title {::om/error {:type :ouch!}}
                     :author {::om/error {:type :oof!}}}
                    {:id 2 :title "Awesome"
                     :author "George"}]}
             (om/get-query UiD))]
    (is (= {:tree {:foo nil}
            :errors {[:ui-e 1] #{{:type :ouch!} {:type :oof!}}}}
           x))
    (is (= {:tree {:foo [nil {:id 2, :title "Awesome", :author "George"}]},
            :errors {[:ui-e 1] #{{:type :ouch!} {:type :oof!}}}}
           ys))))

(deftest test-top-and-contained-errors
  (let [x  (om/default-extract-errors nil
             {:id 0
              ::om/error :yow!
              :foo {:id 1
                    :title {::om/error {:type :ouch!}}
                    :author {::om/error {:type :oof!}}}}
             (om/get-query UiD))
        ys (om/default-extract-errors nil
             {:id 0
              ::om/error :yow!
              :foo [{:id 1
                     :title {::om/error {:type :ouch!}}
                     :author {::om/error {:type :oof!}}}
                    {:id 2 :title "Awesome"
                     :author "George"}]}
             (om/get-query UiD))]
    (is (= {:tree nil
            :errors {[:ui-d 0] #{:yow!},
                     [:ui-e 1] #{{:type :ouch!} {:type :oof!}}}}
           x))
    (is (= {:tree nil
            :errors {[:ui-d 0] #{:yow!}
                     [:ui-e 1] #{{:type :ouch!} {:type :oof!}}}}
           ys))))

(deftest test-simple-union-errors
  (let [x  (om/default-extract-errors nil
             {:id 0
              :bar {:id 1 :type :c
                    :title "Cool"
                    ::om/error {:type :ouch!}}}
             (om/get-query UiF))
        ys (om/default-extract-errors nil
             {:id 0
              :bar [{:id 1 :type :c
                     :title "Cool"
                     ::om/error {:type :ouch!}}]}
             (om/get-query UiF))]
    (is (= {:tree {:bar nil}, :errors {[:ui-c 1] #{{:type :ouch!}}}}
           x))
    (is (= {:tree {:bar [nil]}, :errors {[:ui-c 1] #{{:type :ouch!}}}}
           ys))))

(deftest test-multiple-union-errors
  (let [x  (om/default-extract-errors nil
             {:id 0
              :bar {:id 1 :type :e
                    :title {::om/error {:type :ouch!}}
                    :author {::om/error {:type :oof!}}}}
             (om/get-query UiF))
        ys (om/default-extract-errors nil
             {:id 0
              :bar [{:id 1 :type :e :title "Cool"
                     :author "Bob"}
                    {:id 2 :type :c
                     :title {::om/error {:type :ouch!}}
                     :author {::om/error {:type :oof!}}}
                    {:id 3 :type :e :title "Awesome"
                     :author "George"}]}
             (om/get-query UiF))]
    (is (= {:tree {:bar nil}, :errors {[:ui-e 1] #{{:type :ouch!} {:type :oof!}}}}
           x))
    (is (= {:tree {:bar [{:id 1, :title "Cool", :author "Bob"}
                         nil
                         {:id 3, :title "Awesome", :author "George"}]},
            :errors {[:ui-c 2] #{{:type :ouch!}}}}
           ys))))

(deftest test-union-contained-and-top-errors
  (let [x  (om/default-extract-errors nil
             {:id 0
              ::om/error :yow!
              :bar {:id 1 :type :e
                    :title {::om/error {:type :ouch!}}
                    :author {::om/error {:type :oof!}}}}
             (om/get-query UiF))
        ys (om/default-extract-errors nil
             {:id 0
              ::om/error :yow!
              :bar [{:id 1 :type :c
                     :title "Cool"}
                    {:id 2 :type :e
                     :title {::om/error {:type :ouch!}}
                     :author {::om/error {:type :oof!}}}
                    {:id 2 :type :c
                     :title "Awesome"}]}
             (om/get-query UiF))]
    (is (= {:tree nil
            :errors {[:ui-f 0] #{:yow!}
                     [:ui-e 1] #{{:type :ouch!} {:type :oof!}}}}
           x))
    (is (= {:tree nil
            :errors {[:ui-f 0] #{:yow!}
                     [:ui-e 2] #{{:type :ouch!} {:type :oof!}}}}
           ys))))

;; Mutations

(deftest test-annotate-mutations
  (let [ident [:gizmo/by-id 0]
        tx    '[{:foo [*]} (bar/baz {:woz 2}) {:noz [*]}]]
    (is (= (-> (om/annotate-mutations tx ident)
             second meta :mutator)
           ident))))

(deftest test-mutation-error
  (let [ident [:gizmo/by-id 0]
        x     (om/default-extract-errors nil
                '{do/it! {::om/error {:type :argh!}}}
                (om/annotate-mutations '[(do/it! {:arg0 1})] ident))]
    (is (= {:tree {}, :errors {[:gizmo/by-id 0] #{{:type :argh!}}}} x))))

;; -----------------------------------------------------------------------------
;; Om Merge Internals

(defui Person-Merge
  static om/Ident
  (ident [this {:keys [id]}] [:person/by-id id])
  static om/IQuery
  (query [this]
    [:id :name ]))


(deftest test-merge-idents
  (let [merge-idents #'om/merge-idents                      ; make it non-private for the test
        response     {[:person/by-id 1] {:id 1 :name "Joe"}}
        config       {:normalize true :merge-tree merge :merge-ident om/default-merge-ident}
        query        [{[:person/by-id 1] (om/get-query Person-Merge)}]
        no-query     []
        expected     {:person/by-id {1 {:id 1 :name "Joe"}}}]
    (with-redefs [om/ref->any (fn [idx ident] (om/get-query Person-Merge))]
      (testing "Merge works when a component's query is used for the normalization."
        (is (= expected (merge-idents {} config response no-query)))
        (is (= expected (merge-idents {} config response nil)))))
    (testing "Merge works when a query is passed as an argument."
      (is (= expected (merge-idents {} config response query))))))

;; -----------------------------------------------------------------------------
;; transform-reads

(deftest test-transform-reads-drops-exprs
  (let [r (om/reconciler
            {:state (atom nil)
             :parser (om/parser {:read (fn [_ _ _] {})})})]
    (is (= '[:foo :bar] (om/transform-reads r '[:foo :bar])))
    (with-redefs [om/ref->components (fn [r k]
                                       (if (= k :foo) [nil] []))
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[:foo :baz])
             '[{:foo [:bar]} :baz]))
      (is (= (om/transform-reads r '[(do/it!) :foo :baz])
             '[(do/it!) {:foo [:bar]} :baz]))
      (is (= (om/transform-reads r '[:foo (:baz {:woz :noz})])
            '[{:foo [:bar]} (:baz {:woz :noz})]))
      (is (= (om/transform-reads r '[(do/it!) (do/it!) :foo :baz])
             '[(do/it!) (do/it!) {:foo [:bar]} :baz])))
    (with-redefs [om/ref->components (constantly [nil])
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[:foo :bar])
            '[{:foo [:bar]}]))
      (is (= (om/transform-reads r '[(do/it!) (do/it!) :foo :bar])
             '[(do/it!) (do/it!) {:foo [:bar]}])))))

#?(:cljs
   (deftest test-reconcile!
     ;; OM-766 / OM-768
     (let [update-atom (atom {})
           Comp (ui
                  Object
                  (componentWillReceiveProps [this next-props]
                    (swap! update-atom assoc :componentWillReceiveProps next-props))
                  (render [this]
                    (swap! update-atom assoc :props (om/props this))))
           Root (ui
                  static om/IQuery
                  (query [this]
                    [:foo]))
           r (om/reconciler {:indexer #(om/indexer
                                         {:index-component (fn [indexes component] indexes)
                                          :drop-component  (fn [indexes component] indexes)
                                          :ref->components (fn [{:keys [class->components]} ref]
                                                             (get class->components Comp))})})
           ;; the important thing to note are the `:t`s. In this example we simulate
           ;; a stale Root (< (t root) (t reconciler))
           root (Root. #js {:omcljs$reconciler r
                            :omcljs$path       [:x]
                            :omcljs$value      (om/om-props {:foo 1} 1)})
           c (Comp. #js {:omcljs$reconciler r
                         :omcljs$parent     root
                         :omcljs$value      (om/om-props {:foo 1} 2)})]
       (set! (. c -forceUpdate) (fn [] (swap! update-atom assoc :forceUpdate true)))
       (with-redefs [om/mounted? (constantly true)]
         (p/index-component! (om/get-indexer r) c)
         (swap! (:state r) assoc :queue [:foo] :root root :t 2)
         (p/reconcile! r)
         (is (not (contains? @update-atom :componentWillReceiveProps)))
         (is (true? (:forceUpdate @update-atom)))))
     (let [update-atom (atom {})
           Comp (ui
                  Object
                  (componentWillReceiveProps [this next-props]
                    (swap! update-atom assoc :componentWillReceiveProps next-props))
                  (render [this]
                    (swap! update-atom assoc :props (om/props this))))
           Root (ui
                  static om/IQuery
                  (query [this]
                    [:foo]))
           r (om/reconciler {:indexer #(om/indexer
                                         {:index-component (fn [indexes component] indexes)
                                          :drop-component  (fn [indexes component] indexes)
                                          :ref->components (fn [{:keys [class->components]} ref]
                                                             (get class->components Comp))})})
           ;; In this case, we simulate a state update at Comp, while something
           ;; in Comp's subtree also has `transact!`ed, thus (< (t c) (t reconciler))
           root (Root. #js {:omcljs$reconciler r
                            :omcljs$path       [:x]
                            :omcljs$value      (om/om-props {:foo 1} 1)})
           c (Comp. #js {:omcljs$reconciler r
                         :omcljs$parent     root
                         :omcljs$value      (om/om-props {:foo 1} 1)})]
       (set! (. c -forceUpdate) (fn [] (swap! update-atom assoc :forceUpdate true)))
       (with-redefs [om/mounted? (constantly true)]
         (p/index-component! (om/get-indexer r) c)
         (swap! (:state r) assoc :queue [:foo] :root root :t 2)
         (p/reconcile! r)
         (is (= (-> @update-atom :componentWillReceiveProps)
                {:foo 1}))
         (is (true? (:forceUpdate @update-atom)))))))

(deftest test-tx-listen
  (let [ret (atom [])
        r (om/reconciler {:state (atom {:app/count 0})
                          :parser (om/parser {:mutate (fn [{:keys [state]} _ _]
                                                        {:action (fn []
                                                                   (swap! state update-in [:app/count] inc))})})
                          :tx-listen (fn [tx-data tx]
                                       (swap! ret conj {:tx tx
                                                        :tx-data tx-data}))})]
    (om/transact! r '[(app/inc!)])
    (om/transact! r '[(app/inc!)])
    (is (= (:app/count @r) 2))
    (is (= (-> @ret first :tx-data :old-state) {:app/count 0}))
    (is (= (-> @ret second :tx-data :old-state) {:app/count 1}))
    (is (= (-> @ret second :tx-data :new-state) {:app/count 2}))
    (is (= (-> @ret second :tx :tx) '[(app/inc!)]))
    (is (empty? (-> @ret second :tx :sends)))
    (is (= (-> @ret second :tx :ret) {'app/inc! {:result {:app/count 2}}}))))
