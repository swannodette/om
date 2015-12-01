(ns om.next
  (:refer-clojure :exclude [var? key replace force])
  (:require-macros [om.next :refer [defui]])
  (:require [goog.string :as gstring]
            [goog.object :as gobj]
            [goog.log :as glog]
            [clojure.walk :as walk]
            [om.next.protocols :as p]
            [om.next.impl.parser :as parser]
            [om.next.cache :as c]
            [om.tempid :as tempid]
            [om.transit :as transit]
            [om.util :as util]
            [clojure.zip :as zip])
  (:import [goog.debug Console]))

(defonce *logger*
  (when ^boolean goog.DEBUG
    (.setCapturing (Console.) true)
    (glog/getLogger "om.next")))

;; =============================================================================
;; Globals & Dynamics

(def ^:private roots (atom {}))
(def ^{:dynamic true} *raf* nil)
(def ^{:dynamic true :private true} *reconciler* nil)
(def ^{:dynamic true :private true} *parent* nil)
(def ^{:dynamic true :private true} *shared* nil)
(def ^{:dynamic true :private true} *instrument* nil)
(def ^{:dynamic true :private true} *depth* 0)

;; =============================================================================
;; Utilities

(defn ^boolean nil-or-map? [x]
  (or (nil? x) (map? x)))

(defn- node->key [node]
  (cond
    (map? node) (ffirst node)
    (seq? node) (let [node' (first node)]
                  (when (map? node')
                    (ffirst node')))
    :else nil))

(defn- query-zip [root]
  (zip/zipper
    #(or (vector? %) (map? %) (seq? %))
    seq
    (fn [node children]
      (let [ret (cond
                  (vector? node) (vec children)
                  (map? node)    (into {} children)
                  (seq? node)    children)]
        (with-meta ret (meta node))))
    root))

(defn- move-to-key [loc k]
  (loop [loc (zip/down loc)]
    (let [node (zip/node loc)]
      (if (= k (first node))
        (-> loc zip/down zip/right)
        (recur (zip/right loc))))))

(defn ^boolean union? [node]
  (and (map? node) (< 1 (count node))))

(defn- query-template [query path]
  (letfn [(query-template* [loc path]
            (if (empty? path)
              loc
              (let [node (zip/node loc)]
                (if (vector? node) ;; SUBQUERY
                  (recur (zip/down loc) path)
                  (let [[k & ks] path]
                    (if (union? node)
                      (let [node (zip/node (move-to-key loc k))]
                        (recur (zip/replace loc node) ks)) ;; UNION
                      (let [k' (node->key node)]
                        (if (keyword-identical? k k')
                          (if (map? node)
                            (recur (move-to-key loc k) ks) ;; JOIN
                            (recur (-> loc zip/down zip/down zip/down zip/right) ks)) ;; CALL
                          (recur (zip/right loc) path)))))))))]
    (query-template* (query-zip query) path)))

(defn- replace [template new-query]
  (-> template (zip/replace new-query) zip/root))

(declare focus-query)

(defn- join-key [node]
  (cond
    (map? node) (ffirst node)
    (seq? node) (join-key (first node))
    :else       node))

(defn- join-entry [node]
  (if (seq? node)
    (ffirst node)
    (first node)))

(defn- join-value [join]
  (second (join-entry join)))

(defn- join? [x]
  (let [x (if (seq? x) (first x) x)]
    (map? x)))

(defn- focused-join [node ks]
  (cond
    (map? node) {(ffirst node) (focus-query (-> node first second) ks)}
    (seq? node) (list (focused-join (first node) ks) (second node))
    :else       node))

(defn focus-query
  "Given a query, focus it along the specified path.

  Examples:
    (om.next/focus-query [:foo :bar :baz] [:foo])
    => [:foo]

    (om.next/focus-query [{:foo [:bar :baz]} :woz] [:foo :bar])
    => [{:foo [:bar]}]"
  [query path]
  (if (empty? path)
    query
    (let [[k & ks] path]
      (letfn [(match [x]
                (= k (join-key x)))
              (value [x]
                (focused-join x ks))]
        (if (map? query) ;; UNION
          {k (focus-query (get query k) ks) ::union true}
          (into [] (comp (filter match) (map value) (take 1)) query))))))

;; this function assumes focus is actually in fact
;; already focused!
(defn- focus->path
  ([focus]
   (focus->path focus '* []))
  ([focus bound]
   (focus->path focus bound []))
  ([focus bound path]
   (if (and (or (= bound '*)
                (and (not= path bound)
                     (< (count path) (count bound))))
            (some join? focus)
            (== 1 (count focus)))
     (let [[k focus'] (join-entry (first focus))
           focus'     (if (= '... focus')
                        focus
                        focus')]
       (recur focus' bound (conj path k)))
     path)))

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol Ident
  (ident [this props] "Return the ref for this component"))

(defprotocol IQueryParams
  (params [this] "Return the query parameters"))

(extend-type default
  IQueryParams
  (params [_]))

(defprotocol IQuery
  (query [this] "Return the component's unbound query"))

(defprotocol ILocalState
  (-set-state! [this new-state] "Set the component's local state")
  (-get-state [this] "Get the component's local state")
  (-get-rendered-state [this] "Get the component's rendered local state")
  (-merge-pending-state! [this] "Get the component's pending local state"))

(defn- var? [x]
  (and (symbol? x)
       (gstring/startsWith (name x) "?")))

(defn- var->keyword [x]
  (keyword (.substring (name x) 1)))

(defn- bind-query [query params]
  (letfn [(replace-var [node]
            (if (var? node)
              (get params (var->keyword node) node)
              node))]
    (walk/prewalk replace-var query)))

(declare component? get-reconciler props)

(defn- get-local-query-data [component]
  (some-> (get-reconciler component)
    :config :state deref ::queries (get component)))

(defn get-unbound-query
  "Return the unbound query for a component."
  [component]
  (:query (get-local-query-data component) (query component)))

(defn get-params
  "Return the query params for a component."
  [component]
  (:params (get-local-query-data component) (params component)))

(defn- get-component-query [c]
  (let [qps (get-local-query-data c)
        q   (:query qps (query c))
        c'  (-> q meta :component)]
    (assert (nil? c') (str "Query violation, " c , " reuses " c' " query"))
    (with-meta
      (bind-query q (:params qps (params c)))
      {:component (type c)})))

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also om.next/full-query."
  [x]
  (if (implements? IQuery x)
    (if (component? x)
      (get-component-query x)
      (let [q (query x)
            c (-> q meta :component)]
        (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
        (with-meta (bind-query q (params x)) {:component x})))
    ;; in advanced, statics will get elided
    (when (goog/isFunction x)
      (let [x (js/Object.create (. x -prototype))]
        (when (implements? IQuery x)
          (let [q (query x)
                c (-> q meta :component)]
            (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
            (with-meta (bind-query q (params x)) {:component x})))))))

(defn iquery? [x]
  (implements? IQuery x))

(defn tag [x class]
  (vary-meta x assoc :component class))

;; =============================================================================
;; React Bridging

(deftype ^:private OmProps [props basis-t])

(defn- om-props [props basis-t]
  (OmProps. props basis-t))

(defn- om-props-basis [om-props]
  (.-basis-t om-props))

(def ^:private nil-props (om-props nil -1))

(defn- unwrap [om-props]
  (.-props om-props))

(defn- compute-react-key [cl props]
  (if-let [rk (:react-key props)]
    rk
    (if-let [idx (-> props meta :om-path)]
      (str (. cl -name) "_" idx)
      js/undefined)))

(defn factory
  "Create a factory constructor from a component class created with
   om.next/defui."
  ([class] (factory class nil))
  ([class {:keys [validator keyfn] :as opts}]
   {:pre [(fn? class)]}
   (fn [props & children]
     (when-not (nil? validator)
       (assert (validator props)))
     (if *instrument*
       (apply *instrument* props children)
       (let [key (if-not (nil? keyfn)
                   (keyfn props)
                   (compute-react-key class props))
             ref (:ref props)
             ref (cond-> ref (keyword? ref) str)
             t   (if-not (nil? *reconciler*)
                   (p/basis-t *reconciler*)
                   0)]
         (js/React.createElement class
          #js {:key               key
               :ref               ref
               :omcljs$value      (om-props props t)
               :omcljs$path       (-> props meta :om-path)
               :omcljs$reconciler *reconciler*
               :omcljs$parent     *parent*
               :omcljs$shared     *shared*
               :omcljs$instrument *instrument*
               :omcljs$depth      *depth*}
           (util/force-children children)))))))

(defn ^boolean component?
  "Returns true if the argument is an Om component."
  [x]
  (if-not (nil? x)
    (true? (. x -om$isComponent))
    false))

(defn- state [c]
  {:pre [(component? c)]}
  (.-state c))

(defn- get-prop
  "PRIVATE: Do not use"
  [c k]
  (gobj/get (.-props c) k))

(defn- get-props*
  [x k]
  (if (nil? x)
    nil-props
    (let [y (gobj/get x k)]
      (if (nil? y)
        nil-props
        y))))

(defn- get-prev-props [x]
  (get-props* x "omcljs$prev$value"))

(defn- get-next-props [x]
  (get-props* x "omcljs$next$value"))

(defn- get-props [x]
  (get-props* x "omcljs$value"))

(defn- set-prop!
  "PRIVATE: Do not use"
  [c k v]
  (gobj/set (.-props c) k v))

(defn- get-reconciler
  [c]
  {:pre [(component? c)]}
  (get-prop c "omcljs$reconciler"))

(defn- props*
  ([x y]
   (max-key om-props-basis x y))
  ([x y z]
   (max-key om-props-basis x (props* y z))))

(defn- prev-props*
  ([x y]
   (min-key om-props-basis x y))
  ([x y z]
   (min-key om-props-basis
     (props* x y) (props* y z))))

(defn -prev-props [prev-props component]
  (let [cst   (.-state component)
        props (.-props component)]
    (unwrap
      (prev-props*
        (props* (get-props prev-props) (get-prev-props cst))
        (props* (get-props cst) (get-props props))))))

(defn -next-props [next-props component]
  (unwrap
    (props*
      (get-props next-props)
      (-> component .-props get-props)
      (-> component .-state get-next-props))))

(defn- merge-pending-props! [c]
  {:pre [(component? c)]}
  (let [cst     (. c -state)
        props   (.-props c)
        pending (gobj/get cst "omcljs$next$value")
        prev    (props* (get-props cst) (get-props props))]
    (gobj/set cst "omcljs$prev$value" prev)
    (when-not (nil? pending)
      (gobj/remove cst "omcljs$next$value")
      (gobj/set cst "omcljs$value" pending))))

(defn- clear-prev-props! [c]
  (gobj/remove (.-state c) "omcljs$prev$value"))

(defn- t
  "Get basis t value for when the component last read its props from
   the global state."
  [c]
  (om-props-basis
    (props*
      (-> c .-props get-props)
      (-> c .-state get-props))))

(defn- parent
  "Returns the parent Om component."
  [component]
  (get-prop component "omcljs$parent"))

(defn- depth
  "PRIVATE: Returns the render depth (a integer) of the component relative to
  the mount root."
  [component]
  (when (component? component)
    (get-prop component "omcljs$depth")))

(defn react-key
  "Returns the components React key."
  [component]
  (.. component -props -key))

(defn react-type
  "Returns the component type, regardless of whether the component has been
   mounted"
  [x]
  (or (gobj/get x "type") (type x)))

(defn- path
  "Returns the component's Om data path."
  [c]
  (get-prop c "omcljs$path"))

(defn shared
  "Return the global shared properties of the Om Next root. See :shared and
   :shared-fn reconciler options."
  ([component]
   (shared component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [shared (gobj/get (. component -props) "omcljs$shared")
         ks     (cond-> k-or-ks
                  (not (sequential? k-or-ks)) vector)]
     (cond-> shared
       (not (empty? ks)) (get-in ks)))))

(defn instrument [component]
  {:pre [(component? component)]}
  (get-prop component "omcljs$instrument"))

(defn- update-props! [c next-props]
  {:pre [(component? c)]}
  ;; We cannot write directly to props, React will complain
  (doto (.-state c)
    (gobj/set "omcljs$next$value"
      (om-props next-props (p/basis-t (get-reconciler c))))))

(defn props
  "Return a components props."
  [component]
  {:pre [(component? component)]}
  ;; When force updating we write temporarily props into state to avoid bogus
  ;; complaints from React. We record the basis T of the reconciler to determine
  ;; if the props recorded into state are more recent - props will get updated
  ;; when React actually re-renders the component.
  (unwrap
    (props*
      (-> component .-props get-props)
      (-> component .-state get-props))))

(defn computed
  "Add computed properties to props. Note will replace any pre-existing
   computed properties."
  [props computed-map]
  (when-not (nil? props)
    (if (vector? props)
      (cond-> props
        (not (empty? computed-map)) (vary-meta assoc :om.next/computed computed-map))
      (cond-> props
        (not (empty? computed-map)) (assoc :om.next/computed computed-map)))))

(defn get-computed
  "Return the computed properties on a component or its props."
  ([x]
   (get-computed x []))
  ([x k-or-ks]
   (when-not (nil? x)
     (let [props (cond-> x (component? x) props)
           ks    (into [:om.next/computed]
                   (cond-> k-or-ks
                     (not (sequential? k-or-ks)) vector))]
       (if (vector? props)
         (-> props meta (get-in ks))
         (get-in props ks))))))

(declare schedule-render!)

(defn set-state!
  "Set the component local state of the component. Analogous to React's
   setState."
  [component new-state]
  {:pre [(component? component)]}
  (if (implements? ILocalState component)
    (-set-state! component new-state)
    (gobj/set (.-state component) "omcljs$pendingState" new-state))
  (if-let [r (get-reconciler component)]
    (do
      (p/queue! r [component])
      (schedule-render! r))
    (.forceUpdate component)))

(defn get-state
  "Get a component's local state. May provide a single key or a sequential
   collection of keys for indexed access into the component's local state."
  ([component]
   (get-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if (implements? ILocalState component)
               (-get-state component)
               (when-let [state (. component -state)]
                 (or (gobj/get state "omcljs$pendingState")
                     (gobj/get state "omcljs$state"))))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

(defn update-state!
  "Update a component's local state. Similar to Clojure(Script)'s update-in."
  ([component f]
   (set-state! component (f (get-state component))))
  ([component f arg0]
   (set-state! component (f (get-state component) arg0)))
  ([component f arg0 arg1]
   (set-state! component (f (get-state component) arg0 arg1)))
  ([component f arg0 arg1 arg2]
   (set-state! component (f (get-state component) arg0 arg1 arg2)))
  ([component f arg0 arg1 arg2 arg3]
   (set-state! component (f (get-state component) arg0 arg1 arg2 arg3)))
  ([component f arg0 arg1 arg2 arg3 & arg-rest]
   (set-state! component
     (apply f (get-state component) arg0 arg1 arg2 arg3 arg-rest))))

(defn get-rendered-state
  "Get the rendered state of component. om.next/get-state always returns the
   up-to-date state."
  [component]
  {:pre [(component? component)]}
  (if (implements? ILocalState component)
    (-get-rendered-state component)
    (some-> component .-state (gobj/get "omcljs$state"))))

(defn- merge-pending-state! [c]
  (if (implements? ILocalState c)
    (-merge-pending-state! c)
    (when-let [pending (some-> c .-state (gobj/get "omcljs$pendingState"))]
      (let [state    (.-state c)
            previous (gobj/get state "omcljs$state")]
        (gobj/remove state "omcljs$pendingState")
        (gobj/set state "omcljs$previousState" previous)
        (gobj/set state "omcljs$state" pending)))))

(defn react-set-state!
  ([component new-state]
   (react-set-state! component new-state nil))
  ([component new-state cb]
   {:pre [(component? component)]}
   (.setState component #js {:omcljs$state new-state} nil)))

(declare full-query to-env schedule-sends!)

(defn gather-sends
  [{:keys [parser] :as env} q remotes]
  (into {}
    (comp
      (map #(vector % (parser env q %)))
      (filter (fn [[_ v]] (pos? (count v)))))
    remotes))

(defn set-query!
  "Change the query of a component. Takes a map containing :params and/or
   :query. :params should be a map of new bindings and :query should be a query
   expression. Will schedule a re-render as well as remote re-sends if
   necessary."
  [component {:keys [params query]}]
  {:pre [(component? component)
         (or (not (nil? params))
             (not (nil? query)))]}
  (let [r   (get-reconciler component)
        cfg (:config r)
        st  (:state cfg)
        id  (random-uuid)
        _   (.add (:history cfg) id @st)]
    (when-let [l (:logger cfg)]
      (glog/info l
        (str (when-let [ref (when (implements? Ident component)
                              (ident component (props component)))]
               (str (pr-str ref) " "))
          (when query  "changed query '" query ", ")
          (when params "changed params " params " ")
          (pr-str id))))
    (swap! st update-in [:om.next/queries component] merge
      (merge (when query {:query query}) (when params {:params params})))
    (p/queue! r [component])
    (p/reindex! r)
    (let [sends (gather-sends (to-env cfg)
                  (full-query component) (:remotes cfg))]
      (when-not (empty? sends)
        (p/queue-sends! r sends)
        (schedule-sends! r)))
    nil))

(defn ^boolean mounted?
  "Returns true if the component is mounted."
  [x]
  (and (component? x) ^boolean (.isMounted x)))

(defn react-ref
  "Returns the component associated with a component's React ref."
  [component name]
  (some-> (.-refs component) (gobj/get name)))

(defn children
  "Returns the component's children."
  [component]
  (.. component -props -children))

(defn- update-component! [c next-props]
  {:pre [(component? c)]}
  (update-props! c next-props)
  (.forceUpdate c))

(defn should-update?
  ([c next-props]
   (should-update? c next-props nil))
  ([c next-props next-state]
   {:pre [(component? c)]}
   (.shouldComponentUpdate c
     #js {:omcljs$value next-props}
     #js {:omcljs$state next-state})))

(defn- class-path [c]
  {:pre [(component? c)]}
  (loop [c c ret (list (type c))]
    (if-let [p (parent c)]
      (if (iquery? p)
        (recur p (cons (type p) ret))
        (recur p ret))
      (let [seen (atom #{})]
        (take-while
          (fn [x]
            (when-not (contains? @seen x)
              (swap! seen conj x)
              x))
          ret)))))

(defn subquery
  "Given a class or mounted component x and a ref to an instantiated component
   or class that defines a subquery, pick the most specific subquery. If the
   component is mounted subquery-ref will be used, subquery-class otherwise."
  [x subquery-ref subquery-class]
  {:pre [(or (keyword? subquery-ref) (string? subquery-ref))
         (fn? subquery-class)]}
  (let [ref (cond-> subquery-ref (keyword? subquery-ref) str)]
    (if (and (component? x) (mounted? x))
      (get-query (react-ref x ref))
      (get-query subquery-class))))

(defn get-ident
  "Given a component return its ident"
  [component]
  {:pre [(component? component)]}
  (ident component (props component)))

;; =============================================================================
;; Reconciler API

(declare reconciler?)

(defn- basis-t [reconciler]
  (p/basis-t reconciler))

(defn schedule-render! [reconciler]
  (when (p/schedule-render! reconciler)
    (let [f #(p/reconcile! reconciler)]
      (cond
        (fn? *raf*) (*raf* f)

        (not (exists? js/requestAnimationFrame))
        (js/setTimeout f 16)

        :else
        (js/requestAnimationFrame f)))))

(defn schedule-sends! [reconciler]
  (when (p/schedule-sends! reconciler)
    (js/setTimeout #(p/send! reconciler) 0)))

(declare remove-root!)

(defn add-root!
  "Given a root component class and a target root DOM node, instantiate and
   render the root class using the reconciler's :state property. The reconciler
   will continue to observe changes to :state and keep the target node in sync.
   Note a reconciler may have only one root. If invoked on a reconciler with an
   existing root, the new root will replace the old one."
  ([reconciler root-class target]
   (add-root! reconciler root-class target nil))
  ([reconciler root-class target options]
   {:pre [(reconciler? reconciler) (fn? root-class)]}
   (when-let [old-reconciler (get @roots target)]
     (remove-root! old-reconciler target))
   (swap! roots assoc target reconciler)
   (p/add-root! reconciler root-class target options)))

(defn remove-root!
  "Remove a root target (a DOM element) from a reconciler. The reconciler will
   no longer attempt to reconcile application state with the specified root."
  [reconciler target]
  (p/remove-root! reconciler target))

(defn mock-root
  "Create an Om Next root without an actual DOM target. Useful in devcards
   testing context."
  [reconciler root-class]
  {:pre [(reconciler? reconciler) (fn? root-class)]}
  (p/add-root! reconciler root-class nil nil))

;; =============================================================================
;; Transactions

(defprotocol ITxIntercept
  (tx-intercept [c tx]
    "An optional protocol that component may implement to intercept child
     transactions."))

(defn- to-env [x]
  (let [config (if (reconciler? x) (:config x) x)]
    (select-keys config [:state :shared :parser :logger :pathopt])))

(defn transact* [r c ref tx]
  (let [cfg  (:config r)
        ref  (if (and c (implements? Ident c) (not ref))
               (ident c (props c))
               ref)
        env  (merge
               (to-env cfg)
               {:reconciler r :component c}
               (when ref
                 {:ref ref}))
        id   (random-uuid)
        _    (.add (:history cfg) id @(:state cfg))
        _    (when-let [l (:logger cfg)]
               (glog/info l
                 (str (when ref (str (pr-str ref) " "))
                   "transacted '" tx ", " (pr-str id))))
        v    ((:parser cfg) env tx)
        snds (gather-sends env tx (:remotes cfg))
        q    (cond-> []
               (not (nil? c)) (conj c)
               (not (nil? ref)) (conj ref))]
    (p/queue! r (into q (remove symbol?) (keys v)))
    (when-not (empty? snds)
      (p/queue-sends! r snds)
      (schedule-sends! r))))

(declare ref->components force)

(defn transform-reads [r tx]
  (letfn [(with-target [target q]
            (if-not (nil? target)
              [(force (first q) target)]
              q))
          (add-focused-query [k target tx c]
            (->> (focus-query (get-query c) [k])
              (with-target target)
              (full-query c)
              (into tx)))]
    (loop [exprs (seq tx) tx' []]
      (if-not (nil? exprs)
        (let [expr (first exprs)
              ast  (parser/expr->ast expr)
              key  (:key ast)
              tgt  (:target ast)]
          (if (keyword? key)
            (recur (next exprs)
              (reduce #(add-focused-query key tgt %1 %2)
                tx' (ref->components r key)))
            (recur (next exprs) (conj tx' expr))))
        tx'))))

(defn transact!
  "Given a reconciler or component run a transaction. tx is a parse expression
   that should include mutations followed by any necessary read. The reads will
   be used to trigger component re-rendering. If given a reconciler can be
   optionally passed a ref as the second argument.

   Example:

     (om/transact! widget
       '[(do/this!) (do/that!)
         :read/this :read/that])"
  ([x tx]
   {:pre [(or (component? x)
              (reconciler? x))
          (vector? tx)]}
   (if (reconciler? x)
     (transact* x nil nil tx)
     (do
       (assert (implements? IQuery x)
         (str "transact! invoked by component " x
              " that does not implement IQuery"))
       (loop [p (parent x) x x tx tx]
         (if (nil? p)
           (let [r (get-reconciler x)]
             (transact* r x nil (transform-reads r tx)))
           (let [[x' tx] (if (implements? ITxIntercept p)
                          [p (tx-intercept p tx)]
                          [x tx])]
             (recur (parent p) x' tx)))))))
  ([r ref tx]
   (transact* r nil ref tx)))

;; =============================================================================
;; Parser

(defn parser
  "Create a parser. The argument is a map of two keys, :read and :mutate. Both
   functions should have the signature (Env -> Key -> Params -> ParseResult)."
  [{:keys [read mutate] :as opts}]
  {:pre [(map? opts)]}
  (parser/parser opts))

(defn dispatch
  "Helper function for implementing :read and :mutate as multimethods. Use this
   as the dispatch-fn."
  [_ key _] key)

(defn query->ast
  "Given a query expression convert it into an AST."
  [query-expr]
  (parser/query->ast query-expr))

(defn ast->query [query-ast]
  "Given an AST convert it back into a query expression."
  (parser/ast->expr query-ast true))

;; =============================================================================
;; Indexer

(defrecord Indexer [indexes]
  IDeref
  (-deref [_] @indexes)

  p/IIndexer
  (index-root [_ x]
    (let [prop->classes     (atom {})
          class-path->query (atom {})
          rootq             (get-query x)
          class             (cond-> x (component? x) type)]
      (letfn [(build-index* [class query path classpath]
                (let [recursive? (some #{class} classpath)
                      classpath  (cond-> classpath
                                   (and (not (nil? class))
                                        (not recursive?))
                                   (conj class))]
                  (when class
                    (swap! class-path->query update-in [classpath]
                      (fnil conj #{})
                      (query-template (focus-query rootq path) path)))
                  (when-not recursive?
                    (cond
                      (vector? query)
                      (let [{props false joins true} (group-by join? query)]
                        (when class
                          (swap! prop->classes
                            #(merge-with into %
                              (zipmap
                                (map (comp :dispatch-key parser/expr->ast) props)
                                (repeat #{class})))))
                        (doseq [join joins]
                          (let [[prop query'] (join-entry join)
                                query'        (if (= '... query')
                                                query
                                                query')]
                            (when class
                              (swap! prop->classes
                                #(merge-with into % {prop #{class}})))
                            (let [class' (-> query' meta :component)]
                              (build-index* class' query'
                                (conj path prop) classpath)))))

                      ;; Union query case
                      (map? query)
                      (doseq [[prop query'] query]
                        (let [class' (-> query' meta :component)]
                          (build-index* class' query'
                            (conj path prop) classpath)))))))]
        (build-index* class rootq [] [])
        (swap! indexes merge
          {:prop->classes     @prop->classes
           :class-path->query @class-path->query}))))

  (index-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in indexes
                        [:class->components (type c)]
                        (fnil conj #{}) c)
              ref     (when (implements? Ident c)
                        (ident c (props c)))]
          (if-not (nil? ref)
            (cond-> indexes
              ref (update-in [:ref->components ref] (fnil conj #{}) c))
            indexes)))))

  (drop-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in indexes
                        [:class->components (type c)]
                        disj c)
              ref     (when (implements? Ident c)
                        (ident c (props c)))]
          (if-not (nil? ref)
            (cond-> indexes
              ref (update-in [:ref->components ref] disj c))
            indexes)))))

  (key->components [_ k]
    (let [indexes @indexes]
      (if (component? k)
        #{k}
        (let [cs (get-in indexes [:ref->components k] ::not-found)]
          (if-not (keyword-identical? ::not-found cs)
            cs
            (if (keyword? k)
              ;; TODO: more robust validation, might be bogus key
              (let [cs (get-in indexes [:prop->classes k])]
                (transduce (map #(get-in indexes [:class->components %]))
                  (completing into) #{} cs))
              (throw (js/Error. (str "Invalid key " k ", key must be ref or keyword"))))))))))

(defn indexer
  "Given a function (Component -> Ref), return an indexer."
  []
  (Indexer.
    (atom
      {:class->components {}
       :ref->components   {}})))

(defn get-indexer
  "PRIVATE: Get the indexer associated with the reconciler."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :indexer))

(defn ref->components
  "Return all components for a given ref."
  [x ref]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (p/key->components indexer ref)))

(defn ref->any
  "Get any component from the indexer that matches the ref."
  [x ref]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (p/key->components indexer ref))))

(defn class->any
  "Get any component from the indexer that matches the component class."
  [x class]
  (let [indexer (if (reconciler? x) (get-indexer x) x)]
    (first (get-in @indexer [:class->components class]))))

(defn class-path->query
  [x y]
  (let [indexer (if (reconciler? x) (get-indexer x) x)
        cp      (if (component? y) (class-path y) y)]
    (into #{} (map zip/root)
      (get-in @indexer [:class-path->query cp]))))

(defn full-query
  "Returns the absolute query for a given component, not relative like
   om.next/get-query."
  ([component]
   (when (implements? IQuery component)
     (if (nil? (path component))
       (replace
         (first
           (get-in @(-> component get-reconciler get-indexer)
             [:class-path->query (class-path component)]))
         (get-query component))
       (full-query component (get-query component)))))
  ([component query]
   (when (implements? IQuery component)
     (let [path' (into [] (remove number?) (path component))
           cp    (class-path component)
           qs    (get-in @(-> component get-reconciler get-indexer)
                   [:class-path->query cp])]
       (if-not (empty? qs)
         ;; handle case where child appears multiple times at same class-path
         ;; but with different queries
         (let [q (first (filter #(= path' (-> % zip/root (focus->path path'))) qs))]
           (if-not (nil? q)
             (replace q query)
             (throw
               (ex-info (str "No queries exist for component path " cp " or data path " path')
                 {:type :om.next/no-queries}))))
         (throw
           (ex-info (str "No queries exist for component path " cp)
             {:type :om.next/no-queries})))))))

;; for advanced optimizations
(defn to-class [class]
  (when-not (nil? class)
    (if (not (implements? Ident class))
      (js/Object.create (. class -prototype))
      class)))

(defn ^boolean ident?
  "Returns true if x is an ident."
  [x]
  (and (vector? x)
       (== 2 (count x))
       (keyword? (nth x 0))))

(defn- normalize* [query data refs errs]
  (cond
    (= '[*] query) data

    ;; union case
    (map? query)
    (let [class (to-class (-> query meta :component))
          ref   (when (implements? Ident class)
                  (ident class data))]
      (if-not (nil? ref)
        (vary-meta (normalize* (get query (first ref)) data refs errs)
          assoc :om/tag (first ref))
        (throw (js/Error. "Union components must implement Ident"))))

    (vector? data) data ;; already normalized

    :else
    (loop [q (seq query) ret data]
      (if-not (nil? q)
        (let [node (first q)]
          (if (join? node)
            (let [[k sel] (join-entry node)
                  recursive? (= '... sel)
                  sel     (if recursive?
                            query
                            sel)
                  class   (to-class (-> sel meta :component))
                  v       (get data k)]
              (cond
                ;; graph loop: db->tree leaves ident in place
                (and recursive? (ident? v)) (recur (next q) ret)
                ;; normalize one
                (map? v)
                (let [x (normalize* sel v refs errs)]
                  (if-not (or (nil? class) (not (implements? Ident class)))
                    (let [i (ident class v)]
                      (swap! refs update-in [(first i) (second i)] merge x)
                      (recur (next q) (assoc ret k i)))
                    (recur (next q) (assoc ret k x))))

                ;; normalize many
                (vector? v)
                (let [xs (into [] (map #(normalize* sel % refs errs)) v)]
                  (if-not (or (nil? class) (not (implements? Ident class)))
                    (let [is (into [] (map #(ident class %)) xs)]
                      (if (vector? sel)
                        (when-not (empty? is)
                          (swap! refs update-in [(ffirst is)]
                            (fn [ys]
                              (merge-with merge ys
                                (zipmap (map second is) xs)))))
                        ;; union case
                        (swap! refs
                          (fn [refs']
                            (reduce
                              (fn [ret [i x]]
                                (update-in ret i merge x))
                              refs' (map vector is xs)))))
                      (recur (next q) (assoc ret k is)))
                    (recur (next q) (assoc ret k xs))))

                ;; missing key
                (nil? v)
                (recur (next q) ret)

                ;; can't handle
                :else (recur (next q) (assoc ret k v))))
            (let [k (if (seq? node) (first node) node)
                  v (get data k)]
              (if (nil? v)
                (recur (next q) ret)
                (recur (next q) (assoc ret k v))))))
        ret))))

(defn tree->db
  "Given a Om component class or instance and a tree of data, use the component's
   query to transform the tree into the default database format. All nodes that
   can be mapped via Ident implementations wil be replaced with ident links. The
   original node data will be moved into tables indexed by ident. If merge-ref
   option is true, will return these tables in the result instead of as metadata."
  ([x data]
    (tree->db x data false))
  ([x data ^boolean merge-refs]
   (let [refs (atom {})
         errs (atom {})
         x    (if (vector? x) x (get-query x))
         ret  (normalize* x data refs errs)]
     (if merge-refs
       (let [refs' @refs]
         (assoc (merge ret refs')
           ::tables (into #{} (keys refs'))))
       (with-meta ret @refs)))))

(defn- sift-refs [res]
  (let [{refs true rest false} (group-by #(vector? (first %)) res)]
    [(into {} refs) (into {} rest)]))

;; TODO: easy to optimize

(defn db->tree
  "Given a query, some data in the default database format, and the entire
   application state in the default database format, return the tree where all
   ident links have been replaced with their original node values."
  ([query data refs]
   (db->tree query data refs identity {}))
  ([query data refs map-ident]
   (db->tree query data refs map-ident {}))
  ([query data refs map-ident idents-seen]
   {:pre [(map? refs)]}
    ;; support taking ident for data param
   (let [data (cond-> data (ident? data) (->> map-ident (get-in refs)))]
     (if (vector? data)
       ;; join
       (let [step (fn [ident]
                    (let [ident' (get-in refs (map-ident ident))
                          query' (cond-> query (map? query) (get (first ident)))] ;; UNION
                      (db->tree query' ident' refs map-ident idents-seen)))]
         (into [] (map step) data))
       ;; map case
       (if (= '[*] query)
         data
         (let [{props false joins true} (group-by #(or (join? %) (ident? %)) query)]
           (loop [joins (seq joins) ret {}]
             (if-not (nil? joins)
               (let [join        (first joins)
                     join        (cond-> join (ident? join) (hash-map '[*]))
                     [key sel]   (join-entry join)
                     recurse?    (= '... sel)
                     sel         (if recurse?
                                   query
                                   sel)
                     v           (if (ident? key)
                                   (if (= '_ (second key))
                                     (get refs (first key))
                                     (get-in refs (map-ident key)))
                                   (get data key))
                     key         (cond-> key
                                   (and (ident? key) (= '_ (second key)))
                                   first)
                     v           (if (ident? v) (map-ident v) v)
                     graph-loop? (and recurse? (contains? (set (get idents-seen key)) v))
                     idents-seen (if recurse? (-> idents-seen
                                                  (update-in [key] (fnil conj #{}) v)
                                                  (assoc-in [:last-ident key] v)) idents-seen)]
                 (cond
                   graph-loop? (recur (next joins) ret)
                   (nil? v)    (recur (next joins) ret)
                   :else       (recur (next joins)
                                (assoc ret key (db->tree sel v refs map-ident idents-seen)))))
               (if-let [looped-key (some (fn [[k identset]]
                                           (if (contains? identset (get data k))
                                             (get-in idents-seen [:last-ident k])
                                             nil)) (dissoc idents-seen :last-ident))]
                 looped-key
                 (merge (select-keys data props) ret))))))))))

;; =============================================================================
;; Reconciler

(defn rewrite [paths]
  (fn [res]
    (letfn [(step [res [k orig-path]]
              (-> res
                (dissoc k)
                (assoc-in orig-path (get res k))))]
      (reduce step res paths))))

(defn process-roots
  "A send helper for rewriting the query to remove client local keys that
   don't need server side processing. Give a query this function will
   return a map with two keys, :query and :rewrite. :query is the
   actual query you should send. Upon receiving the response you should invoke
   :rewrite on the response before invoking the send callback."
  [query]
  (letfn [(process-roots* [query state path]
            (when (vector? query)
              (loop [ks (seq query)]
                (if-not (nil? ks)
                  (let [k (first ks)]
                    (if (true? (-> k meta :query-root))
                      (swap! state
                        #(let [jk (join-key k)]
                          (-> %
                            (update-in [:query] conj k)
                            (assoc-in [:paths jk] (conj path jk)))))
                      (do
                        (when (join? k)
                          (let [[jk jv] (join-entry k)]
                            (process-roots* jv state (conj path jk))))
                        (recur (next ks)))))))))]
    (let [state (atom {:query [] :paths {}})
          _     (process-roots* query state [])
          st    @state]
      (if-not (empty? (:query st))
        (assoc st :rewrite (rewrite (:paths st)))
        {:query query :rewrite identity}))))

(defn- merge-refs [tree config refs]
  (let [{:keys [merge-ref indexer]} config]
    (letfn [(step [tree' [ref props]]
              (if (:normalize config)
                (let [c      (ref->any indexer ref)
                      props' (tree->db c props)
                      refs   (meta props')]
                  ((:merge-tree config) (merge-ref config tree' ref props') refs))
                (merge-ref config tree' ref props)))]
      (reduce step tree refs))))

(defn- merge-novelty!
  [reconciler state res]
  (let [config      (:config reconciler)
        root        (:root @(:state reconciler))
        [refs res'] (sift-refs res)
        res'        (if (:normalize config)
                      (tree->db root res' true)
                      res')]
    (-> state
      (merge-refs config refs)
      ((:merge-tree config) res'))))

(defn default-merge [reconciler state res]
  {:keys    (into [] (remove symbol?) (keys res))
   :next    (merge-novelty! reconciler state res)
   :tempids (->> (filter (comp symbol? first) res)
              (map (comp :tempids second))
              (reduce merge {}))})

(defn merge!
  "Merge a state delta into the application state. Affected components managed
   by the reconciler will re-render."
  [reconciler delta]
  (let [config (:config reconciler)
        state  (:state config)
        merge  (:merge config)
        {:keys [keys next tempids]} (merge reconciler @state delta)]
    (p/queue! reconciler keys)
    (reset! state
      (if-let [migrate (:migrate config)]
        (migrate next (get-query (:root @(:state reconciler)))
          tempids (:id-key config))
        next))))

(defrecord Reconciler [config state]
  IDeref
  (-deref [this] @(:state config))

  p/IReconciler
  (basis-t [_] (:t @state))

  (add-root! [this root-class target options]
    (let [ret   (atom nil)
          rctor (factory root-class)
          guid  (random-uuid)]
      (when (implements? IQuery root-class)
        (p/index-root (:indexer config) root-class))
      (when (and (:normalize config)
                 (not (:normalized @state)))
        (let [new-state (tree->db root-class @(:state config))
              refs      (meta new-state)]
          (reset! (:state config) (merge new-state refs))
          (swap! state assoc :normalized true)))
      (let [renderf (fn [data]
                      (binding [*reconciler* this
                                *shared*     (merge
                                               (:shared config)
                                               (when (:shared-fn config)
                                                 ((:shared-fn config) data)))]
                        (let [c (cond
                                  (not (nil? target)) ((:root-render config) (rctor data) target)
                                  (nil? @ret) (rctor data)
                                  :else (when-let [c' @ret]
                                          (when (mounted? c')
                                            (.forceUpdate c' data))))]
                          (when (and (nil? @ret) (not (nil? c)))
                            (swap! state assoc :root c)
                            (reset! ret c)))))
            parsef  (fn []
                      (let [sel (get-query (or @ret root-class))]
                        (assert (or (nil? sel) (vector? sel))
                          "Application root query must be a vector")
                        (if-not (nil? sel)
                          (let [env (to-env config)
                                v   ((:parser config) env sel)]
                            (when-not (empty? v)
                              (renderf v)))
                          (renderf @(:state config)))))]
        (swap! state merge
          {:target target :render parsef :root root-class
           :remove (fn []
                     (remove-watch (:state config) (or target guid))
                     (swap! state
                       #(-> %
                         (dissoc :target) (dissoc :render) (dissoc :root)
                         (dissoc :remove)))
                     (when-not (nil? target)
                       ((:root-unmount config) target)))})
        (add-watch (:state config) (or target guid)
          (fn [_ _ _ _]
            (swap! state update-in [:t] inc)
            (schedule-render! this)))
        (parsef)
        (when-let [sel (get-query (or (and target @ret) root-class))]
          (let [env  (to-env config)
                snds (gather-sends env sel (:remotes config))]
            (when-not (empty? snds)
              (when-let [send (:send config)]
                (send snds
                  #(do
                    (merge! this %)
                    (renderf ((:parser config) env sel))))))))
        @ret)))

  (remove-root! [_ target]
    (when-let [remove (:remove @state)]
      (remove)))

  (reindex! [_]
    (let [root (get @state :root)]
      (when (implements? IQuery root)
        (p/index-root (:indexer config) root))))

  (queue! [_ ks]
    (swap! state update-in [:queue] into ks))

  (queue-sends! [_ sends]
    (swap! state update-in [:queued-sends]
      (:merge-sends config) sends))

  (schedule-render! [_]
    (if-not (:queued @state)
      (swap! state update-in [:queued] not)
      false))

  (schedule-sends! [_]
    (if-not (:sends-queued @state)
      (do
        (swap! state assoc [:sends-queued] true)
        true)
      false))

  ;; TODO: need to reindex roots after reconcilation
  (reconcile! [_]
    (let [st @state
          q  (:queue st)]
      (swap! state update-in [:queued] not)
      (swap! state assoc :queue [])
      (cond
        ;; TODO: need to move root re-render logic outside of batching logic
        (empty? q) ((:render st))

        (some (comp zero? depth) q) ((:render st))

        (= [::skip] q) nil

        :else
        (let [cs (transduce
                   (map #(p/key->components (:indexer config) %))
                   #(into %1 %2) #{} q)
              {:keys [ui->props]} config
              env (to-env config)]
          (doseq [c ((:optimize config) cs)]
            (when (mounted? c)
              (let [computed   (get-computed (props c))
                    next-props (om.next/computed (ui->props env c) computed)]
                (when (should-update? c next-props (get-state c))
                  (if-not (nil? next-props)
                    (update-component! c next-props)
                    (.forceUpdate c))))))))))

  (send! [this]
    (let [sends (:queued-sends @state)]
      (when-not (empty? sends)
        (swap! state
          (fn [state]
            (-> state
              (assoc :queued-sends {})
              (assoc :sends-queued false))))
        ((:send config) sends #(merge! this %))))))

(defn- default-ui->props
  [{:keys [parser ^boolean pathopt] :as env} c]
  (let [ui (when (and pathopt (implements? Ident c) (iquery? c))
             (let [id (ident c (props c))]
               (get (parser env [{id (get-query c)}]) id)))]
    (if-not (nil? ui)
      ui
      (let [fq (full-query c)]
        (when-not (nil? fq)
          (let [s  (system-time)
                ui (parser env fq)
                e  (system-time)]
            (when-let [l (:logger env)]
              (let [dt (- e s)]
                (when (< 16 dt)
                  (glog/warning l (str c " query took " dt " msecs")))))
            (get-in ui (path c))))))))

(defn- default-merge-ref
  [_ tree ref props]
  (update-in tree ref merge props))

(defn- default-merge-tree
  [a b]
  (if (map? a)
    (merge a b)
    b))

(defn- default-migrate [pure query tempids id-key]
  (letfn [(dissoc-in [pure [table id]]
            (assoc pure table (dissoc (get pure table) id)))
          (step [pure [old [_ id :as new]]]
            (-> pure
              (dissoc-in old)
              (assoc-in new
                (cond-> (merge (get-in pure old) (get-in pure new))
                  (not (nil? id-key)) (assoc id-key id)))))]
    (if-not (empty? tempids)
      (let [pure' (reduce step pure tempids)]
        (tree->db query
          (db->tree query pure' pure'
            (fn [ident] (get tempids ident ident))) true))
      pure)))

(defn reconciler
  "Construct a reconciler from a configuration map.

   Required parameters:
     :state        - the application state. If IAtom value is not supplied the
                     data will be normalized into the default database format
                     using the root query. This can be disabled by explicitly
                     setting the optional :normalize parameter to false.
     :parser       - the parser to be used

   Optional parameters:
     :shared       - a map of global shared properties for the component tree.
     :shared-fn    - a function to compute global shared properties from the root props.
                     the result is merged with :shared.
     :send         - required only if the parser will return a non-empty value when
                     run against the supplied :remotes. send is a function of two
                     arguments, the map of remote expressions keyed by remote target
                     and a callback which should be invoked with the result from each
                     remote target. Note this means the callback can be invoked
                     multiple times to support parallel fetching and incremental
                     loading if desired.
     :normalize    - whether the state should be normalized. If true it is assumed
                     all novelty introduced into the system will also need
                     normalization.
     :remotes      - a vector of keywords representing remote services which can
                     evaluate query expressions. Defaults to [:remote]
     :root-render  - the root render function. Defaults to ReactDOM.render
     :root-unmount - the root unmount function. Defaults to
                     ReactDOM.unmountComponentAtNode
     :logger       - supply a goog.log compatible logger"
  [{:keys [state shared shared-fn
           parser indexer
           ui->props normalize
           send merge-sends remotes
           merge merge-tree merge-ref
           optimize
           history
           root-render root-unmount
           pathopt
           migrate id-key]
    :or {ui->props    default-ui->props
         indexer      om.next/indexer
         merge-sends  #(merge-with into %1 %2)
         remotes      [:remote]
         merge        default-merge
         merge-tree   default-merge-tree
         merge-ref    default-merge-ref
         optimize     (fn [cs] (sort-by depth cs))
         history      100
         root-render  #(js/ReactDOM.render %1 %2)
         root-unmount #(js/ReactDOM.unmountComponentAtNode %)
         pathopt      false
         migrate      default-migrate}
    :as config}]
  {:pre [(map? config)]}
  (let [idxr   (indexer)
        norm?  (satisfies? IAtom state)
        state' (if norm? state (atom state))
        logger (if (contains? config :logger)
                 (:logger config)
                 *logger*)
        ret    (Reconciler.
                 {:state state' :shared shared :shared-fn shared-fn
                  :parser parser :indexer idxr
                  :ui->props ui->props
                  :send send :merge-sends merge-sends :remotes remotes
                  :merge merge :merge-tree merge-tree :merge-ref merge-ref
                  :optimize optimize
                  :normalize (or (not norm?) normalize)
                  :history (c/cache history)
                  :root-render root-render :root-unmount root-unmount
                  :logger logger :pathopt pathopt
                  :migrate migrate :id-key id-key}
                 (atom {:queue [] :queued false :queued-sends {}
                        :sends-queued false
                        :target nil :root nil :render nil :remove nil
                        :t 0 :normalized false}))]
    ret))

(defn ^boolean reconciler?
  "Returns true if x is a reconciler."
  [x]
  (instance? Reconciler x))

(defn app-state
  "Return the reconciler's application state atom. Useful when the reconciler
   was initialized via denormalized data."
  [reconciler]
  (-> reconciler :config :state))

(defn from-history
  "Given a reconciler and UUID return the application state snapshost
   from history associated with the UUID. The size of the reconciler history
   may be configured by the :history option when constructing the reconciler."
  [reconciler uuid]
  (.get (-> reconciler :config :history) uuid))

(defn tempid
  "Return a temporary id."
  ([] (tempid/tempid))
  ([id] (tempid/tempid id)))

(defn reader
  "Create a Om Next transit reader. This reader can handler the tempid type.
   Can pass transit reader customization opts map."
  ([] (transit/reader))
  ([opts] (transit/reader opts)))

(defn writer
  "Create a Om Next transit writer. This writer can handler the tempid type.
   Can pass transit writer customization opts map."
  ([] (transit/writer))
  ([opts] (transit/writer opts)))

(defn force
  "Given a query expression return an equivalent query expression that can be
   spliced into a transaction that will force a read of that key from the
   specified remote target."
  ([expr]
    (force expr :remote))
  ([expr target]
    (with-meta (list 'quote expr) {:target target})))
