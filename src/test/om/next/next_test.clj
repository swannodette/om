(ns om.next.next-test
  (:require [clojure.test :refer [deftest testing is are]]
            [om.next :as om :refer [defui ui]]
            [om.dom :as dom]
            [om.next.protocols :as p])
  (:import [om.next Indexer]))

(defui SimpleComponent
  Object
  (initLocalState [this]
    {:foo 1}))

(def simple-component-factory (om/factory SimpleComponent))

(deftest test-get-prop
  (is (= (#'om/get-prop (simple-component-factory) :omcljs$depth) 0))
  (binding [om/*shared* :fake]
    (is (= (#'om/get-prop (simple-component-factory) :omcljs$shared) :fake))))

(defui ComponentWithStatics
  static om/Ident
  (ident [this props]
    [:by-id 42])
  static om/IQuery
  (query [this]
    [:foo]))

(deftest test-defui
  (testing "defui definition works"
    (is SimpleComponent)
    (is (fn? SimpleComponent)))
  (testing "defui implements Lifecycle protocols"
    (let [c (SimpleComponent nil nil nil nil)]
      (is (om/component? c))
      (is (= (.initLocalState c) {:foo 1}))))
  (let [c (ComponentWithStatics nil nil nil nil)]
    (testing "defui implements statics"
      (is (= (.query c) [:foo]))
      (is (= (.ident c {}) [:by-id 42])))
    (testing "allow defui not to implement lifecycle render"
      (is (om/component? c))
      (is (nil? (.render c)))))
  (is (fn? (-> SimpleComponent meta :component)))
  (testing "`ui` macro"
    (is (= (om/get-query
             (ui
               static om/IQuery
               (query [this] [:foo])))
           [:foo]))))

(deftest test-component?-predicate
  (let [simple-c-factory (om/factory SimpleComponent)
        c (simple-c-factory)]
    (is (om/component? c))
    (is (not (om/component? simple-c-factory)))
    (is (not (om/component? SimpleComponent)))))

(defui ReactKeysChild
  static om/IQuery
  (query [this]
    [:name])
  Object
  (render [this]
    (let [p (om/props this)]
      (dom/div nil (:name p)))))

(def react-keys-child-factory (om/factory ReactKeysChild))

(defui ReactKeysParent
  static om/IQuery
  (query [this]
    [{:children (om/get-query ReactKeysChild)}])
  Object
  (render [this]
    (let [children (:children (om/props this))]
      (dom/div nil
        (map react-keys-child-factory children)))))

(def react-keys-state
  {:children [{:name "John"} {:name "Mary"}]})

(defn react-keys-read
  [{:keys [state]} _ _]
  {:value (:children @state)})

(deftest test-react-bridging
  (testing "factory, state, props, children"
    (let [c (simple-component-factory)]
      (is (satisfies? p/IReactComponent c))
      (is (= (om/get-state c) {:foo 1}))
      (is (= (om/get-state c :foo) 1))
      (is (= (om/get-state c [:foo]) 1))
      (is (= (om/get-rendered-state c :foo) 1))
      (is (= (om/get-rendered-state c [:foo]) 1))
      (is (= (om/get-state c) (om/get-rendered-state c)))
      (om/set-state! c {:bar 1})
      (is (= (om/get-state c) {:bar 1}))
      (om/update-state! c #(update-in % [:bar] inc))
      (is (= (om/get-state c) {:bar 2}))
      (is (= (om/props c) nil))
      (is (= (om/props (simple-component-factory {:foo 1})) {:foo 1}))
      (is (= (om/children (simple-component-factory nil "some text"))
            ["some text"]))))
  (testing "react keys"
    (let [c (simple-component-factory {:react-key "foo"})
          r (om/reconciler {:state (atom react-keys-state)
                                    :parser (om/parser {:read react-keys-read})})
          rks-c (om/add-root! r ReactKeysParent nil)]
      (is (= (om/react-key c) "foo"))
      (is (= (-> (p/-render rks-c) :children first :react-key)
             "om$next$next_test$ReactKeysChild_[:children 0]"))))
  (testing "react type"
    (let [c (simple-component-factory)]
      (is (thrown? AssertionError (om/react-type 42)))
      (is (thrown? AssertionError (om/react-type nil)))
      (is (= (om/react-type c) SimpleComponent)))))

(deftest test-factory-validator
  (let [f (om/factory SimpleComponent {:validator (fn [props] (some? props))})]
    (is (thrown? AssertionError (f nil)))))

(deftest test-computed-props
  (is (= (om/get-computed (om/computed {} {:a 1}))
         {:a 1}))
  (is (= (om/get-computed (om/computed {:some :prop} {:a 1}))
         {:a 1}))
  (is (= (om/computed {:some :prop} {:a 1})
         {:om.next/computed {:a 1} :some :prop})))

(defui ComponentWithQPs
  static om/IQueryParams
  (params [this]
    {:a 1})
  static om/IQuery
  (query [this]
    '[:foo (:bar {:a ?a})]))

(defui ComponentWithQuery
  static om/IQuery
  (query [this]
    [:foo :bar]))

(defui SubqueryChild
  static om/IQuery
  (query [this]
    [:foo :bar])
  Object
  (render [this]
    (dom/div nil "I'm a child")))

(def subquery-child-factory (om/factory SubqueryChild))

(defui SubqueryParent
  static om/IQuery
  (query [this]
    [{:children (om/get-query SubqueryChild)}])
  Object
  (render [this]
    (dom/div nil
      (subquery-child-factory {:ref :child-1})
      (subquery-child-factory {:ref :child-2}))))

(deftest test-queries
  (let [cqps-factory (om/factory ComponentWithQPs)
        cq-factory (om/factory ComponentWithQuery)
        cqps (cqps-factory)
        cq (cq-factory)]
    (testing "iquery?"
      (is (om/iquery? ComponentWithQPs))
      (is (om/iquery? cqps))
      (is (not (om/iquery? SimpleComponent))))
    (testing "get-query"
      (is (= (om/get-query ComponentWithQPs)
             '[:foo (:bar {:a 1})]))
      (is (= (meta (om/get-query ComponentWithQPs))
             {:component ComponentWithQPs}))
      #_(is (= (om/get-query cqps)
             '[:foo (:bar {:a 1})]))
      #_(is (= (meta (om/get-query cqps))
             {:component ComponentWithQPs}))
      (is (= (om/get-query ComponentWithQuery)
             [:foo :bar]))
      #_(is (= (om/get-query cq)
             [:foo :bar]))
      (is (= (om/get-unbound-query cq)
             [:foo :bar]))
      (is (= (om/get-unbound-query cqps)
             '[:foo (:bar {:a ?a})]))
      (is (= (om/params ComponentWithQPs)
             {:a 1}))
      (is (nil? (om/params SimpleComponent)))
      (is (= (om/get-params cqps)
             {:a 1}))))
  (testing "subquery"
    (let [factory (om/factory SubqueryParent)
          c (factory)]
      (is (= (om/subquery c :child-1 SubqueryChild)
             [:foo :bar]))
      (p/-render c)
      (is (#'om/mounted? c))
      (is (om/react-ref c :child-1))
      (is (#'om/mounted? (om/react-ref c :child-1)))
      #_(is (= (om/subquery c :child-1 SubqueryChild)
             [:foo :bar])))))

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
         [:people/list [:person/by-id 0] :person/name])))

(deftest test-temp-id-equality
  (let [uuid (java.util.UUID/randomUUID)
        id0  (om/tempid uuid)
        id1  (om/tempid uuid)]
    (is (om/tempid? id0))
    (is (= id0 id1))
    (is (= (hash id0) (hash id1)))))

(defui ComponentWithIdent
  static om/Ident
  (ident [this {:keys [id]}]
    [:item/by-id id]))

(deftest test-get-ident
  (let [factory (om/factory ComponentWithStatics)
        c (factory {})]
    (is (= (om/get-ident c) [:by-id 42])))
  (let [factory (om/factory ComponentWithIdent)
        c (factory {:id 3})]
    (is (= (om/get-ident c) [:item/by-id 3]))))

(defui ReactRefsChild
  Object
  (render [this]
    (dom/div nil "some text")))

(def child-factory (om/factory ReactRefsChild))

(defui ReactRefsParent
  Object
  (render [this]
    (dom/div nil
      (child-factory {:ref "foo"}))))

(deftest test-react-refs
  (let [factory (om/factory ReactRefsParent)
        c (factory)]
    (p/-render c)
    (is (= (om/react-type
             (om/react-ref c "foo"))
           ReactRefsChild))))

(deftest test-dom-node
  (let [factory (om/factory ReactRefsParent)
        c (factory)
        sb (StringBuilder.)]
    (p/-render c)
    (is (instance? om.dom.Element (dom/node c)))
    (is (instance? om.dom.Element (dom/node c "foo")))
    (dom/render-element! (dom/node c "foo") (volatile! 1) sb)
    (is (= (str sb)
           "<div data-reactroot=\"\" data-reactid=\"1\">some text</div>"))))

(defui ClassPathChild
  static om/IQuery
  (query [this]
    [:foo :bar])
  Object
  (render [this]
    (dom/div nil "stuff")))

(def class-path-child-factory (om/factory ClassPathChild))

(defui ClassPathParent
  static om/IQuery
  (query [this]
    [{:child (om/get-query ClassPathChild)}])
  Object
  (render [this]
    (class-path-child-factory {:ref "child"})))

(deftest test-class-path
  (let [factory (om/factory ClassPathParent)
        c (factory)]
    (p/-render c)
    (is (= (om/class-path (om/react-ref c "child"))
           [ClassPathParent ClassPathChild]))))

(deftest test-reconciler
  (let [r (om/reconciler {:state {:a 1}})]
    (is (om/reconciler? r))
    (is (= @(om/app-state r) {:a 1}))))

;; Indexer

(defui ^:once Component
  static om/IQuery
  (query [this]
    '[:foo/bar :baz/woz]))

(defui ComponentList
  static om/IQueryParams
  (params [this]
    {:component (om/get-query Component)})
  static om/IQuery
  (query [this]
    '[{:components/list ?component} :app/title]))

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

(deftest test-reindex-instances
  (let [r (om/reconciler
            {:state (atom nil)
             :parser (om/parser {:read (fn [_ _ _] {})})})
        idxr (get-in r [:config :indexer])
        ;; simulate mounting
        _ (p/add-root! r RootComponent nil nil)
        _ (p/index-component! idxr (RootComponent nil nil #js {:omcljs$reconciler r} nil))
        indexes @(:indexes idxr)
        classes (-> indexes :class->components keys)
        cps (-> indexes :class-path->query keys)
        c (first (get-in indexes [:class->components RootComponent]))]
    (is (= (first classes) RootComponent))
    (is (not (nil? (some #{[RootComponent ComponentA]} cps))))
    ;; will reindex
    (om/set-query! c {:params {:component (om/get-query ComponentB)}})
    (let [indexes @(:indexes idxr)
          cps (-> indexes :class-path->query keys)]
      (is (not (nil? (some #{[RootComponent ComponentB]} cps)))))))

(deftest test-reconciler-has-indexer
  (let [r (om/reconciler
            {:state (atom nil)})]
    (is (instance? Indexer (get-in r [:config :indexer])))))

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

;; Remote test

(defn remote-read
  [{:keys [state query]} k _]
  (assert (= k :some/list))
  (let [st @state]
    (if (contains? st k)
      {:value (om/db->tree query (get st k) st)}
      {:remote true})))

(defui RemotePerson
  static om/Ident
  (ident [this {:keys [name]}]
    [:person/by-name name])
  static om/IQuery
  (query [this]
    [:name :age])
  Object
  (render [this]
    (let [{:keys [name age] :as props} (om/props this)]
      (dom/li nil
        (dom/label nil (str name ", age: " age))))))

(def remote-person (om/factory RemotePerson {:keyfn :name}))

(defui RemoteList
  static om/IQuery
  (query [this]
    [{:some/list (om/get-query RemotePerson)}])
  Object
  (render [this]
    (let [{:keys [some/list]} (om/props this)]
      (apply dom/ul nil
        (map remote-person list)))))

(defn remote-send [{:keys [remote]} cb]
  ;; simulate calling the server parser
  (cb {:some/list [{:name "John" :age 30} {:name "Mary" :age 25}]} remote))

(def remote-reconciler
  (om/reconciler {:state (atom {})
                          :normalize true
                          :parser (om/parser {:read remote-read})
                          :send remote-send}))

(deftest test-remote-send
  (let [c (om/add-root! remote-reconciler RemoteList nil)]
    (is (some? c))
    (is (not (empty? @remote-reconciler)))))

(defui MigratePerson
  static om/Ident
  (ident [this {:keys [db/id]}]
    [:person/by-id id])
  static om/IQuery
  (query [this]
    [:db/id :person/name]))

(defui MigratePeople
  static om/IQuery
  (query [this]
    [{:people (om/get-query MigratePerson)}]))

(defn migrate-read
  [{:keys [state query]} k _]
  (let [st @state]
    {:value (om/db->tree query (get st k) st)}))

(deftest test-migrate
  (let [tmpid (om/tempid (java.util.UUID/randomUUID))
        r (om/reconciler {:state (atom {:people [[:person/by-id tmpid]]
                                                :person/by-id  {tmpid {:db/id tmpid
                                                                       :person/name "Joe"}}})
                                  :normalize true
                                  :id-key :db/id
                                  :parser (om/parser {:read migrate-read})})]
    (om/add-root! r MigratePeople nil)
    (is (om/tempid? (-> @r :person/by-id ffirst)))
    (om/merge! r
      {'some/action! {:tempids {[:person/by-id tmpid] [:person/by-id 42]}}}
      (om/get-query MigratePeople))
    (is (not (om/tempid? (-> @r :person/by-id ffirst))))
    (is (= (-> @r :person/by-id ffirst) 42))))

(defui InstrumentChild
  Object
  (render [this]
    (dom/p nil "I'm a child")))

(def instrument-child (om/factory InstrumentChild {:instrument? true}))

(defui InstrumentRoot
  Object
  (render [this]
    (dom/div nil
      (instrument-child))))

(deftest test-instrument
  (let [cnt (atom 0)
        r (om/reconciler
            {:state (atom {:root 1})
             :instrument (fn [{:keys [class props children factory]}]
                           (swap! cnt inc)
                           (apply factory props children))})]
    (dom/render-to-str (om/add-root! r InstrumentRoot nil))
    (is (= @cnt 2))))

(defmulti transact-read om/dispatch)
(defmulti transact-mutate om/dispatch)

(defmethod transact-read :app/count
  [{:keys [state]} k _]
  {:value (get @state k)})

(defmethod transact-mutate 'this/throws
  [_ _ _]
  {:action #(throw (Exception.))})

(defmethod transact-mutate 'app/inc!
  [{:keys [state]} _ _]
  {:action #(swap! state update-in [:app/count] inc)})

(deftest test-transact!
  (let [r (om/reconciler {:state (atom {:app/count 0})
                          :parser (om/parser {:read transact-read
                                                      :mutate transact-mutate})})
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

;; Bugs

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

(deftest test-om-727
  (let [data {:item [:bar 0]
              :bar    {0 {:id 0 :next [:bar 1]}
                       1 {:id 1 :next [:bar 2]}
                       2 {:id 2}}}]
    (is (= (om/db->tree [{:item {:foo [:id] :bar [:id {:next '...}]}}] data data)
           {:item {:id 0, :next {:id 1, :next {:id 2}}}}))))

(deftest test-om-732
  (let [state {:curr-view [:main :view]
               :main {:view {:curr-item [[:sub-item/by-id 2]]}}
               :sub-item/by-id {2 {:foo :baz :sub-items [[:sub-item/by-id 4]]}
                                4 {:foo :bar}}}]
    (is (= (om/db->tree [{:curr-view
                          {:main [{:curr-item [:foo {:sub-items '...}]}]}}] state state)
           {:curr-view {:curr-item [{:foo :baz :sub-items [{:foo :bar}]}]}}))))

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

;; -----------------------------------------------------------------------------
;; tranform-reads

(deftest test-transform-reads-drops-exprs
  (let [r (om/reconciler
            {:state (atom nil)
             :parser (om/parser {:read #(do {})})})]
    (is (= '[:foo :bar] (om/transform-reads r '[:foo :bar])))
    (with-redefs [om/ref->components (fn [r k]
                                       (if (= k :foo) [nil] []))
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[:foo :baz])
             '[{:foo [:bar]} :baz])))
    (with-redefs [om/ref->components (fn [r k]
                                       (if (= k :foo) [nil] []))
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[:foo (:baz {:woz :noz})])
             '[{:foo [:bar]} (:baz {:woz :noz})])))
    (with-redefs [om/ref->components (constantly [nil])
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[:foo :bar])
             '[{:foo [:bar]}])))
    (with-redefs [om/ref->components (fn [r k]
                                       (if (= k :foo) [nil] []))
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[(do/it!) :foo :baz])
             '[(do/it!) {:foo [:bar]} :baz])))
    (with-redefs [om/ref->components (fn [r k]
                                       (if (= k :foo) [nil] []))
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[(do/it!) (do/it!) :foo :baz])
             '[(do/it!) (do/it!) {:foo [:bar]} :baz])))
    (with-redefs [om/ref->components (constantly [nil])
                  om/get-query (constantly [{:foo [:bar]}])
                  om/full-query (constantly [{:foo [:bar]}])]
      (is (= (om/transform-reads r '[(do/it!) (do/it!) :foo :bar])
             '[(do/it!) (do/it!) {:foo [:bar]}])))))

(deftest test-remote-send-no-return
  (let [r (om/reconciler {:state (atom {:foo "Hello, "})
                          :parser (om/parser
                                    {:read (fn [{:keys [state target]} k _]
                                             (merge {:value (get @state k)}
                                               (when (= k :bar)
                                                 {:remote true})))})
                          :send (fn [{:keys [remote]} cb]
                                  ;; simulate calling the server parser
                                  (cb {:bar " world."}))})
       RemoteRoot (ui
                    static om/IQuery
                    (query [this]
                      [:foo :bar])
                    Object
                    (render [this]
                      (let [{:keys [foo bar] :as props} (om/props this)]
                        (is (= {:foo "Hello, " :bar " world."} props))
                        (dom/div nil (str foo bar)))))
       c (om/add-root! r RemoteRoot nil)]
    (dom/render-to-str c)
    (is (some? c))
    (is (not (empty? @r)))))

(defui NonLifecycleMethodsComponent
  Object
  (someMethod [this]
    "Hello World")
  (render [this]
    (.someMethod this)))

(deftest test-defui-non-lifecycle-methods
  (let [c ((om/factory NonLifecycleMethodsComponent))]
    (is (= (.someMethod c) "Hello World"))
    (is (= (.render c) "Hello World"))))

(deftest extracting-protocol-static-methods
  (testing "extracts protocols with single methods"
    (is (= '{:params (fn [this]), :query (fn ([this] [:a])), :ident (fn ([this props] [:a 1]))}
          (#'om/extract-static-methods '[IQuery (query [this] [:a])
                                         Ident (ident [this props] [:a 1])]))))
  (testing "extracts protocols with multiple methods"
    (is (= '{:params   (fn [this]), :query (fn ([this] [:a])), :ident (fn ([this props] [:a 1]))
             :method-a (fn ([this] :a)) :method-b (fn ([this] :b)) :method-c (fn ([this] :c))}
          (#'om/extract-static-methods '[IQuery (query [this] [:a]) Multi
                                         (method-a [this] :a)
                                         (method-b [this] :b)
                                         (method-c [this] :c)
                                         Ident
                                         (ident [this props] [:a 1])]))))
  (testing "extracts protocols with multiple arity methods"
    (is (= '{:params (fn [this]) :method-a (fn ([this] :b) ([this arg] :a))}
          (#'om/extract-static-methods '[Multi
                                         (method-a [this arg] :a)
                                         (method-a [this] :b)])))))

(deftest test-om-877-recursive-edge-case
  (let [query [{:tree [:id
                       :value
                       {:children '...}]}]
        state {:tree {:id 0 :value 42
                      :children [{:id 1 :value 43
                                  :children [{:id 2 :value 99
                                              :children []}]}
                                 {:id 3 :value 101
                                  :children []}]}}]
    (is (= state
           (om/db->tree query state state)))))
