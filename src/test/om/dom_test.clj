(ns om.dom-test
  (:refer-clojure :exclude [read])
  (:require [clojure.test :refer [deftest testing is are]]
            [om.test-utils :refer [remove-whitespace]]
            [om.next :as om :refer [defui]]
            [om.dom :as dom]
            [om.next.protocols :as p]))

(defn test-tags [tags res-fn]
  `(are [element# res#] (let [sb# (StringBuilder.)]
                          (dom/render-element! {:tag element#} (volatile! 1) sb#)
                          (= (str sb#) res#))
     ~@(mapcat (fn [tag#] [tag# (res-fn tag#)]) tags)))

(defmacro test-container-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(dom/container-tag? % nil)))]
    (test-tags container-tags #(str "<" % " data-reactroot=\"\" data-reactid=\"1\">" "</" % ">"))))

(defmacro test-void-tags []
  (let [container-tags (->> dom/tags
                         (map str)
                         (filter #(not (dom/container-tag? % nil))))]
    (test-tags container-tags #(str "<" % " data-reactroot=\"\" data-reactid=\"1\"/>"))))

(defn simple-component []
  (dom/div nil "Hello World"))

(defn simple-nested-component []
  (dom/div nil
    (dom/h1 #js {:id "page-title"} "Title")))

(defn comp-nested-component []
  (dom/div nil
    (simple-component)
    (simple-nested-component)))

(deftest test-render-element
  (testing "render-element works with empty content in all tags"
    (test-container-tags)
    (test-void-tags))
  (testing "render-element renders simple function elements"
    (are [component res] (let [sb (StringBuilder.)]
                           (dom/render-element! component (volatile! 1) sb)
                           (= (str sb) res))
      (simple-component) "<div data-reactroot=\"\" data-reactid=\"1\">Hello World</div>"
      (simple-nested-component) (remove-whitespace
                                  "<div data-reactroot=\"\" data-reactid=\"1\">
                                     <h1 id=\"page-title\" data-reactid=\"2\">Title</h1>
                                   </div>")
      (comp-nested-component) (remove-whitespace
                                "<div data-reactroot=\"\" data-reactid=\"1\">
                                   <div data-reactid=\"2\">Hello World</div>
                                   <div data-reactid=\"3\">
                                     <h1 id=\"page-title\" data-reactid=\"4\">Title</h1>
                                   </div>
                                 </div>"))))

(defui SimpleComponent
  Object
  (render [this]
    (dom/div nil "Hello World")))

(defui Hello
  Object
  (render [this]
    (dom/p nil (-> this om/props :text))))

(defui Children
  Object
  (render [this]
    (dom/div nil
      (map identity
        #js [(dom/div nil "Foo")
             (dom/div nil "Bar")
             (map identity
               #js [(dom/div nil "Bar")
                    (dom/div nil "Woz")])]))))

(deftest test-render-to-str
  (let [c ((om/factory SimpleComponent))]
    (is (= (str (#'dom/render-to-str* c)) "<div data-reactroot=\"\" data-reactid=\"1\">Hello World</div>")))
  (let [hello (om/factory Hello)]
    (is (= (str (#'dom/render-to-str* (hello {:text "Hello, world!"})))
           "<p data-reactroot=\"\" data-reactid=\"1\">Hello, world!</p>")))
  (let [children (om/factory Children)]
    (is (= (str (#'dom/render-to-str* (children)))
          (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                  <div data-reactid=\"2\">Foo</div>
                                  <div data-reactid=\"3\">Bar</div>
                                  <div data-reactid=\"4\">Bar</div>
                                  <div data-reactid=\"5\">Woz</div>
                              </div>")))))

(deftest test-format-react-attrs
  (are [map res] (let [sb (StringBuilder.)]
                   (dom/render-attr-map! sb "div" map)
                   (= (str sb) res))
    {:htmlFor "something"} " for=\"something\""
    {:className "foo"} " class=\"foo\""
    {:srcLang "en"} " srclang=\"en\""
    {:acceptCharset "ISO-8859-1"} " accept-charset=\"ISO-8859-1\""
    {:placeholder "Title"} " placeholder=\"Title\""
    ;; svg xlink:stuff
    {:xlinkActuate "foo"} " xlink:actuate=\"foo\""))

(deftest test-ref-is-elided-in-props
  (let [sb (StringBuilder.)]
    (dom/render-element! (dom/div #js {:ref "someDiv"}) (volatile! 1) sb)
    (is (= (str sb)
          "<div data-reactroot=\"\" data-reactid=\"1\"></div>"))))

(deftest test-attrs-rendered-in-declaration-order
  (are [element res] (let [sb (StringBuilder.)]
                       (dom/render-element! element (volatile! 1) sb)
                       (= (str sb) res))
    (dom/input {:type "text"
                :placeholder "some text"
                :id "stuff"})
    "<input type=\"text\" placeholder=\"some text\" id=\"stuff\" data-reactroot=\"\" data-reactid=\"1\"/>"

    (dom/input {:id "stuff"
                :placeholder "some text"
                :type "text"})
    "<input type=\"text\" id=\"stuff\" placeholder=\"some text\" data-reactroot=\"\" data-reactid=\"1\"/>"

    (dom/input {:placeholder "some text"
                :id "stuff"
                :type "text"})
    "<input type=\"text\" placeholder=\"some text\" id=\"stuff\" data-reactroot=\"\" data-reactid=\"1\"/>"))

(deftest test-only-supported-attrs-rendered
  (are [element markup] (= (str (#'dom/render-to-str* element)) (remove-whitespace markup))
    (dom/div #js {:not-supported "foo"}) "<div data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div {:className "stuff" :class "other"}) "<div class=\"stuff\" data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div {:media :stuff}) "<div data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div {:data-foo "foo"}) "<div data-foo=\"foo\" data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div {:foo true}) "<div data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div {:autoFocus true}) "<div autofocus data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/div #js {:autoCapitalize true
                  :color "tomato"
                  :itemScope 1
                  :keyParams true})
    "<div autocapitalize color=\"tomato\" itemscope=\"1\" keyparams data-reactroot=\"\" data-reactid=\"1\"></div>"
    (dom/svg #js {:panose1 "stuff"}) "<svg panose-1=\"stuff\" data-reactroot=\"\" data-reactid=\"1\"></svg>"))


(def styles
  #js {:textAlign "center"
       :marginLeft "10px"})

(defui ComponentWithStyle
  Object
  (render [this]
    (dom/div #js {:style styles})))

(deftest test-normalize-styles
  (are [styles result] (let [sb (StringBuilder.)]
                         (dom/normalize-styles! sb styles)
                         (= (str sb) result))
    (select-keys styles [:textAlign]) "text-align:center;"
    styles "text-align:center;margin-left:10px;"
    {:zoom 1} "zoom:1;"
    {:zoom 1
     :opacity 0.5
     :width 100} "zoom:1;opacity:0.5;width:100px;"))

(deftest test-empty-styles-not-rendered
  (let [sb (StringBuilder.)]
    (dom/render-element! (dom/div {:style {}}) (volatile! 1) sb)
    (is (= (str sb)
          "<div data-reactroot=\"\" data-reactid=\"1\"></div>"))))

(deftest test-render-component-with-style
  (let [ctor (om/factory ComponentWithStyle)]
    (is (= (str (#'dom/render-to-str* (ctor)))
          "<div style=\"text-align:center;margin-left:10px;\" data-reactroot=\"\" data-reactid=\"1\"></div>"))))

;; Simple nested `defui`s

(defui SimpleNestedChild
  Object
  (render [this]
    (dom/div nil "child")))

(def simple-nested-child-factory (om/factory SimpleNestedChild))

(defui SimpleNestedParent
  Object
  (render [this]
    (dom/div nil
      (simple-nested-child-factory))))

(deftest test-simple-nested-defuis
  (let [ctor (om/factory SimpleNestedParent)]
    (is (= (str (#'dom/render-to-str* (ctor)))
           (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                   <div data-reactid=\"2\">child</div>
                               </div>")))))


;; Om Simple Recursive Tree
(def simple-tree-data
  {:tree {:node-value 1
          :children [{:node-value 2
                      :children [{:node-value 3
                                  :children []}]}
                     {:node-value 4
                      :children []}]}})

(declare simple-node)

(defui SimpleNode
  static om/IQuery
  (query [this]
    '[:node-value {:children ...}])
  Object
  (render [this]
    (let [{:keys [node-value children]} (om/props this)]
      (dom/li nil
        (dom/div nil (str "Node value:" node-value))
        (dom/ul nil
          (map simple-node children))))))

(def simple-node (om/factory SimpleNode))

(defui SimpleTree
  static om/IQuery
  (query [this]
    [{:tree (om/get-query SimpleNode)}])
  Object
  (render [this]
    (let [{:keys [tree]} (om/props this)]
      (dom/ul nil
        (simple-node tree)))))

(defmulti simple-tree-read om/dispatch)

(defmethod simple-tree-read :node-value
  [{:keys [data] :as env} _ _]
  {:value (:node-value data)})

(defmethod simple-tree-read :children
  [{:keys [data parser query] :as env} _ _]
  {:value (let [f #(parser (assoc env :data %) query)]
            (into [] (map f (:children data))))})

(defmethod simple-tree-read :tree
  [{:keys [state parser query] :as env} k _]
  (let [st @state]
    {:value (parser (assoc env :data (:tree st)) query)}))

(def simple-tree-reconciler
  (om/reconciler
    {:state     (atom simple-tree-data)
     :normalize false
     :parser    (om/parser {:read simple-tree-read})}))

(deftest test-render-simple-recursive-example
  (let [c (om/add-root! simple-tree-reconciler SimpleTree nil)]
    (is (= (str (#'dom/render-to-str* c))
           (remove-whitespace
             "<ul data-reactroot=\"\" data-reactid=\"1\">
                  <li data-reactid=\"2\">
                    <div data-reactid=\"3\">Node value:1</div>
                    <ul data-reactid=\"4\">
                      <li data-reactid=\"5\">
                        <div data-reactid=\"6\">Node value:2</div>
                        <ul data-reactid=\"7\">
                          <li data-reactid=\"8\">
                            <div data-reactid=\"9\">Node value:3</div>
                            <ul data-reactid=\"10\"></ul>
                          </li>
                        </ul>
                      </li>
                      <li data-reactid=\"11\">
                        <div data-reactid=\"12\">Node value:4</div>
                        <ul data-reactid=\"13\"></ul>
                      </li>
                    </ul>
                  </li>
              </ul>")))))

(defn MultipleTextChildren []
  (dom/div nil
    "Some text"
    "More text"))

(defn ChildAndText []
  (dom/div nil
    (dom/p nil "A paragraph!")
    "More text"))

(deftest test-render-multiple-text-children
  (testing "rendering an element with multiple children converts text nodes to <span>"
    (are [comp res] (let [sb (StringBuilder.)]
                      (dom/render-element! (comp) (volatile! 1) sb)
                      (= (str sb)
                         (remove-whitespace res)))
      MultipleTextChildren "<div data-reactroot=\"\" data-reactid=\"1\">
                              <!-- react-text: 2 -->Some text<!-- /react-text -->
                              <!-- react-text: 3 -->More text<!-- /react-text -->
                            </div>"
      ChildAndText "<div data-reactroot=\"\" data-reactid=\"1\">
                      <p data-reactid=\"2\">A paragraph!</p>
                      <!-- react-text: 3 -->More text<!-- /react-text -->
                    </div>")))

;; Shared test

(defui Home
  static om/IQuery
  (query [this] [:counter])

  Object
  (render [this]
    (let [shared (om/shared this)
          props  (om/props this)]
      (dom/div nil
        (dom/h3 nil (str "Props: " props))
        (dom/h3 nil (str "Shared: " shared))
        (dom/button
          #js {:onClick #(om/transact! this '[(my/test) :counter])}
          "Increment!")))))

(def app-state (atom {:counter 0}))

(defn read
  [env key params]
  (let [{:keys [state]} env]
    {:value (get @state key)}))

(defn mutate
  [env key params]
  (let [{:keys [state]} env]
    {:value  {:keys [:counter]}
     :action #(swap! state update-in [:counter] inc)}))

(def reconciler
  (om/reconciler
    {:state     app-state
     :parser    (om/parser {:read read :mutate mutate})
     :shared    {}
     :shared-fn (fn [root-props]
                  root-props)}))

(deftest test-shared
  (let [c (om/add-root! reconciler Home nil)]
    (is (= (str (#'dom/render-to-str* c))
           (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                   <h3 data-reactid=\"2\">Props: {:counter 0}</h3>
                                   <h3 data-reactid=\"3\">Shared: {:counter 0}</h3>
                                   <button data-reactid=\"4\">Increment!</button>
                               </div>")))))

(deftest test-render-to-str-elements
  (are [elem res] (= (str (#'dom/render-to-str* elem)) res)
    (dom/div nil "foo") "<div data-reactroot=\"\" data-reactid=\"1\">foo</div>"))

(deftest react-key-in-elements
  (is (= (:react-key (dom/div {:key "foo"})) "foo"))
  (is (= (:attrs (dom/div {:key "foo"})) {}))
  (is (= (:react-key (dom/div nil)) nil))
  (is (= (str (#'dom/render-to-str* (dom/div {:key "foo"})))
        "<div data-reactroot=\"\" data-reactid=\"1\"></div>"))
  (is (= (str (#'dom/render-to-str* (dom/div nil (dom/div #js {:key "foo"}))))
        "<div data-reactroot=\"\" data-reactid=\"1\"><div data-reactid=\"2\"></div></div>")))

(deftest test-non-string-attributes
  (is (= (str (#'dom/render-to-str* (dom/div {:className 3})))
        "<div class=\"3\" data-reactroot=\"\" data-reactid=\"1\"></div>")))

(defui NilChild
  Object
  (render [this]
    nil))

(def nil-child-factory (om/factory NilChild))

(defui NilParent
  Object
  (render [this]
    (dom/div nil
      "foo"
      (nil-child-factory)
      "bar")))

(defui NilChildrenComp
  Object
  (render [this]
    (dom/div #js {}
      nil)))

(deftest test-nil-children
  (is (= (str (#'dom/render-to-str* (nil-child-factory)))
         "<!-- react-empty: 1 -->"))
  (is (= (str (#'dom/render-to-str* ((om/factory NilChildrenComp))))
         "<div data-reactroot=\"\" data-reactid=\"1\"></div>"))
  (is (= (str (#'dom/render-to-str* ((om/factory NilParent))))
         (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                               <!-- react-text: 2 -->foo<!-- /react-text -->
                               <!-- react-empty: 3 -->
                               <!-- react-text: 4 -->bar<!-- /react-text --></div>"))))

(defui CLPHN-3-Component-1
  Object
  (initLocalState [this]
    {:a 1})
  (componentWillMount [this]
    (om/update-state! this update-in [:a] inc))
  (render [this]
    (dom/div nil (str "a: " (om/get-state this :a)))))

(defui CLPHN-3-Component-2
  Object
  (initLocalState [this]
    {:a 1})
  (componentWillMount [this]
    (om/update-state! this update-in [:a] inc))
  (render [this]
    (dom/div nil (str "a: " (om/get-state this :a)))))

(defui CLPHN-3-Child
  Object
  (initLocalState [this]
    {:a 1})
  (componentWillMount [this]
    (om/update-state! this update-in [:a] inc))
  (render [this]
    (dom/div nil (str "child a: " (om/get-state this :a)))))

(def clphn3-child (om/factory CLPHN-3-Child))

(defui CLPHN-3-Parent
  Object
  (initLocalState [this]
    {:a 2})
  (componentWillMount [this]
    (om/update-state! this update-in [:a] inc))
  (render [this]
    (dom/div nil
      (clphn3-child)
      (str "parent a: " (om/get-state this :a)))))

(deftest test-clphn-3
  (let [c1 ((om/factory CLPHN-3-Component-1))
        c2 ((om/factory CLPHN-3-Component-2))
        c3 ((om/factory CLPHN-3-Parent))]
    (is (= (str (#'dom/render-to-str* c1))
           "<div data-reactroot=\"\" data-reactid=\"1\">a: 2</div>"))
    (is (= (str (#'dom/render-to-str* c2))
           "<div data-reactroot=\"\" data-reactid=\"1\">a: 2</div>"))
    (is (= (str (#'dom/render-to-str* c3))
           (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\">
                                 <div data-reactid=\"2\">child a: 2</div>
                                 <!-- react-text: 3 -->parent a: 3<!-- /react-text -->
                               </div>")))))

(defui SomeChild
  Object
  (render [this]
    (dom/div nil "foo")))

(def some-child (om/factory SomeChild))

(defui SomeParent
  Object
  (render [this]
    (some-child)))

(deftest test-om-644
  (is (= (str (#'dom/render-to-str* ((om/factory SomeParent))))
         "<div data-reactroot=\"\" data-reactid=\"1\">foo</div>")))

;; React 15

(defui React15Comp
  Object
  (render [this]
    (dom/div nil
      (dom/div nil
        "nested"
        (dom/div nil "other")))))

(deftest react-15-render
  (is (= (dom/render-to-str ((om/factory React15Comp)))
         (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\" data-react-checksum=\"1635398171\">
                               <div data-reactid=\"2\">
                                 <!-- react-text: 3 -->nested<!-- /react-text -->
                                 <div data-reactid=\"4\">other</div>
                               </div>
                             </div>")))
  (is (= (dom/render-to-str
           (dom/div nil
             (dom/div nil
               (dom/div nil "3"))
             (dom/div nil
               (dom/div nil "5"))))
        (remove-whitespace "<div data-reactroot=\"\" data-reactid=\"1\" data-react-checksum=\"-1239993276\">
                              <div data-reactid=\"2\">
                                <div data-reactid=\"3\">3</div>
                              </div>
                              <div data-reactid=\"4\">
                                <div data-reactid=\"5\">5</div>
                              </div>
                            </div>"))))

(deftest render-react-fragment
  (is (= (str (#'dom/render-to-str*
               (dom/div #js {}
                (dom/<> #js {}
                        (dom/input #js {:value "foo" :id "bar" :type "text"})
                        (dom/input #js {:value "foo" :id "bar" :type "text"})))))
         "<div data-reactroot=\"\" data-reactid=\"1\"><input type=\"text\" value=\"foo\" id=\"bar\" data-reactid=\"2\"/><input type=\"text\" value=\"foo\" id=\"bar\" data-reactid=\"3\"/></div>")))

(deftest render-wrapped-attrs
  (is (= (str (#'dom/render-to-str* (dom/input #js {:value "foo" :id "bar" :type "text"})))
         "<input type=\"text\" value=\"foo\" id=\"bar\" data-reactroot=\"\" data-reactid=\"1\"/>"))
  (is (= (str (#'dom/render-to-str* (dom/option #js {:disabled "" :label "foo" :selected ""})))
         "<option selected=\"\" disabled=\"\" label=\"foo\" data-reactroot=\"\" data-reactid=\"1\"></option>"))
  ;; https://github.com/facebook/react/commit/fc0431
  (is (= (str (#'dom/render-to-str* (dom/input #js {:value "foo" :step 3 :name "points" :type "number"})))
         "<input type=\"number\" step=\"3\" value=\"foo\" name=\"points\" data-reactroot=\"\" data-reactid=\"1\"/>"))
  (is (= (str (#'dom/render-to-str* (dom/input #js {:value "foo" :type "number" :name "points" :step 3})))
         "<input type=\"number\" step=\"3\" value=\"foo\" name=\"points\" data-reactroot=\"\" data-reactid=\"1\"/>"))
  ;; https://github.com/facebook/react/commit/3013af
  (is (= (str (#'dom/render-to-str* (dom/input #js {:max 42 :value "foo" :min 10 :type "number" :name "points" :step 3})))
         "<input type=\"number\" step=\"3\" min=\"10\" max=\"42\" value=\"foo\" name=\"points\" data-reactroot=\"\" data-reactid=\"1\"/>")))

(deftest test-om-800
  (let [sb (StringBuilder.)]
    (p/-render-to-string (dom/text-node "foo's") 0 sb)
    (is (= (str sb) "foo&#x27;s")))
  (let  [sb (StringBuilder.)]
    (p/-render-to-string (dom/react-text-node "foo's") (volatile! 0) sb)
    (is (= (str sb) (str "<!-- react-text: 0 -->foo&#x27;s<!-- /react-text -->")))))

(deftest test-om-818
  (let  [sb (StringBuilder.)]
    (p/-render-to-string (dom/script #js {:type "text/javascript"
                                          :dangerouslySetInnerHTML {:__html "'foo bar'"}})
      (volatile! 0) sb)
    (is (= (str sb) "<script type=\"text/javascript\" data-reactid=\"0\">'foo bar'</script>"))))

(deftest test-om-799
  (let [elem (dom/create-element "use")
        sb (StringBuilder.)]
    (is (instance? om.dom.Element elem))
    (p/-render-to-string elem (volatile! 0) sb)
    (is (= (str sb) "<use data-reactid=\"0\"></use>")))
  (let [sb (StringBuilder.)]
    (p/-render-to-string (dom/create-element "use" nil "use-child") (volatile! 0) sb)
    (is (= (str sb) "<use data-reactid=\"0\">use-child</use>")))
  (let [elem (dom/create-element "use"
               #js {:key "foo"
                    :x "50" :y "10"
                    :xlinkHref "#Port"}
               "use-child")
        sb (StringBuilder.)]
    (p/-render-to-string elem (volatile! 0) sb)
    (is (= (str sb) "<use x=\"50\" y=\"10\" xlink:href=\"#Port\" data-reactid=\"0\">use-child</use>"))))
