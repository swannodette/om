(ns om.next
  (:refer-clojure :exclude [var? key replace force])
  (:require-macros [om.next :refer [defui invariant]])
  (:require [goog.string :as gstring]
            [goog.object :as gobj]
            [goog.log :as glog]
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

(defn- expr->key
  "Given a query expression return its key."
  [expr]
  (cond
    (keyword? expr) expr
    (map? expr)     (ffirst expr)
    (seq? expr)     (let [expr' (first expr)]
                      (when (map? expr')
                        (ffirst expr')))
    (util/ident? expr)   (cond-> expr (= '_ (second expr)) first)
    :else
    (throw
      (ex-info (str "Invalid query expr " expr)
        {:type :error/invalid-expression}))))

(defn- query-zip
  "Return a zipper on a query expression."
  [root]
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

(defn- move-to-key
  "Move from the current zipper location to the specified key. loc must be a
   hash map node."
  [loc k]
  (loop [loc (zip/down loc)]
    (let [node (zip/node loc)]
      (if (= k (first node))
        (-> loc zip/down zip/right)
        (recur (zip/right loc))))))

(defn- query-template
  "Given a query and a path into a query return a zipper focused at the location
   specified by the path. This location can be replaced to customize / alter
   the query."
  [query path]
  (letfn [(query-template* [loc path]
            (if (empty? path)
              loc
              (let [node (zip/node loc)]
                (if (vector? node) ;; SUBQUERY
                  (recur (zip/down loc) path)
                  (let [[k & ks] path
                        k' (expr->key node)]
                    (if (keyword-identical? k k')
                      (if (or (map? node)
                              (and (seq? node) (map? (first node))))
                        (let [loc'  (move-to-key (cond-> loc (seq? node) zip/down) k)
                              node' (zip/node loc')]
                          (if (map? node') ;; UNION
                            (if (seq ks)
                              (recur
                                (zip/replace loc'
                                  (zip/node (move-to-key loc' (first ks))))
                                (next ks))
                              loc')
                            (recur loc' ks))) ;; JOIN
                        (recur (-> loc zip/down zip/down zip/down zip/right) ks)) ;; CALL
                      (recur (zip/right loc) path)))))))]
    (query-template* (query-zip query) path)))

(defn- replace [template new-query]
  (-> template (zip/replace new-query) zip/root))

(declare focus-query*)

(defn- focused-join [expr ks full-expr union-expr]
  (let [expr-meta (meta expr)
        expr' (cond
                (map? expr)
                (let [join-value (-> expr first second)
                      join-value (if (and (util/recursion? join-value)
                                          (seq ks))
                                   (if-not (nil? union-expr)
                                     union-expr
                                     full-expr)
                                   join-value)]
                  {(ffirst expr) (focus-query* join-value ks nil)})

                (seq? expr) (list (focused-join (first expr) ks nil nil) (second expr))
                :else       expr)]
    (cond-> expr'
      (some? expr-meta) (with-meta expr-meta))))

(defn- focus-query*
  [query path union-expr]
  (if (empty? path)
    query
    (let [[k & ks] path]
      (letfn [(match [x]
                (= k (util/join-key x)))
              (value [x]
                (focused-join x ks query union-expr))]
        (if (map? query) ;; UNION
          {k (focus-query* (get query k) ks query)}
          (into [] (comp (filter match) (map value) (take 1)) query))))))

(defn focus-query
  "Given a query, focus it along the specified path.

  Examples:
    (om.next/focus-query [:foo :bar :baz] [:foo])
    => [:foo]

    (om.next/focus-query [{:foo [:bar :baz]} :woz] [:foo :bar])
    => [{:foo [:bar]}]"
  [query path]
  (focus-query* query path nil))

;; this function assumes focus is actually in fact
;; already focused!
(defn- focus->path
  "Given a focused query return the path represented by the query.

   Examples:

     (om.next/focus->path [{:foo [{:bar {:baz []}]}])
     => [:foo :bar :baz]"
  ([focus]
   (focus->path focus '* []))
  ([focus bound]
   (focus->path focus bound []))
  ([focus bound path]
   (if (and (or (= bound '*)
                (and (not= path bound)
                     (< (count path) (count bound))))
            (some util/join? focus)
            (== 1 (count focus)))
     (let [[k focus'] (util/join-entry (first focus))
           focus'     (if (util/recursion? focus')
                        focus
                        focus')]
       (recur focus' bound (conj path k)))
     path)))

;; =============================================================================
;; Query Protocols & Helpers

(defprotocol Ident
  (ident [this props] "Return the ident for this component"))

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
       (gstring/startsWith (str x) "?")))

(defn- var->keyword [x]
  (keyword (.substring (str x) 1)))

(defn- replace-var [expr params]
  (if (var? expr)
    (get params (var->keyword expr) expr)
    expr))

(defn- bind-query [query params]
  (let [qm (meta query)
        tr (map #(bind-query % params))
        ret (cond
              (seq? query) (apply list (into [] tr query))
              (coll? query) (into (empty query) tr query)
              :else (replace-var query params))]
    (cond-> ret
      (and qm (satisfies? IMeta ret)) (with-meta qm))))

(declare component? get-reconciler props class-path get-indexer path)

(defn- component->query-data [component]
  (some-> (get-reconciler component)
    :config :state deref ::queries (get component)))

(defn get-unbound-query
  "Return the unbound query for a component."
  [component]
  (:query (component->query-data component) (query component)))

(defn get-params
  "Return the query params for a component."
  [component]
  (:params (component->query-data component) (params component)))

(defn- get-component-query
  ([component]
   (get-component-query component (component->query-data component)))
  ([component query-data]
   (let [q  (:query query-data (query component))
         c' (-> q meta :component)]
     (assert (nil? c')
       (str "Query violation, " component " reuses " c' " query"))
     (with-meta
       (bind-query q (:params query-data (params component)))
       {:component (type component)}))))

(defn ^boolean iquery? [x]
  (implements? IQuery x))

(defn- get-class-or-instance-query
  "Return a IQuery/IParams local bound query. Works for component classes
   and component instances. Does not use the indexer."
  [x]
  (if (component? x)
    (get-component-query x)
    (let [q (query x)
          c (-> q meta :component)]
      (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
      (with-meta (bind-query q (params x)) {:component x}))))

(defn- get-indexed-query
  "Get a component's static query from the indexer. For recursive queries, recurses
   up the data path. Falls back to `get-class-or-instance-query` if nothing is
   found in the indexer."
  [component class-path-query-data data-path]
  (let [qs (filter #(= data-path (-> % zip/root (focus->path data-path)))
             class-path-query-data)
        qs (if (empty? qs) class-path-query-data qs)]
    (if-not (empty? qs)
      (let [q (first qs)
            node (zip/node q)]
        (if-not (util/recursion? node)
          node
          (recur component class-path-query-data (pop data-path))))
      (get-class-or-instance-query component))))

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also om.next/full-query."
  [x]
  (when (implements? IQuery x)
    (if (component? x)
      (if-let [query-data (component->query-data x)]
        (get-component-query x query-data)
        (let [cp (class-path x)
              r (get-reconciler x)
              data-path (into [] (remove number?) (path x))
              class-path-query-data (get (:class-path->query @(get-indexer r)) cp)]
          (get-indexed-query x class-path-query-data data-path)))
      (get-class-or-instance-query x))))

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
  ([class {:keys [validator keyfn instrument?]
           :or {instrument? true} :as opts}]
   {:pre [(fn? class)]}
   (fn self [props & children]
     (when-not (nil? validator)
       (assert (validator props)))
     (if (and *instrument* instrument?)
       (*instrument*
         {:props    props
          :children children
          :class    class
          :factory  (factory class (assoc opts :instrument? false))})
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
               :omcljs$reactKey   key
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
      (-> component .-props get-props)
      (get-props next-props)
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
  (get-prop component "omcljs$reactKey"))

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
  "Update a component's local state. Similar to Clojure(Script)'s swap!"
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
  ([component]
   (get-rendered-state component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [cst (if (implements? ILocalState component)
               (-get-rendered-state component)
               (some-> component .-state (gobj/get "omcljs$state")))]
     (get-in cst (if (sequential? k-or-ks) k-or-ks [k-or-ks])))))

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
   (.setState component #js {:omcljs$state new-state} cb)))

(declare full-query to-env schedule-sends! reconciler? ref->components force)

(defn gather-sends
  [{:keys [parser] :as env} q remotes]
  (into {}
    (comp
      (map #(vector % (parser env q %)))
      (filter (fn [[_ v]] (pos? (count v)))))
    remotes))

(defn transform-reads
  "Given r (a reconciler) and a query expression including a mutation and
   any simple reads, return the equivalent query expression where the simple
   reads have been replaced by the full query for each component that cares about
   the specified read."
  [r tx]
  (if (-> r :config :easy-reads)
    (letfn [(with-target [target q]
             (if-not (nil? target)
               [(force (first q) target)]
               q))
           (add-focused-query [k target tx c]
             (let [transformed (->> (focus-query (get-query c) [k])
                                 (with-target target)
                                 (full-query c))]
               (into tx (remove (set tx)) transformed)))]
     (loop [exprs (seq tx) tx' []]
       (if-not (nil? exprs)
         (let [expr (first exprs)
               ast (parser/expr->ast expr)
               key (:key ast)
               tgt (:target ast)]
           (if (keyword? key)
             (let [cs (ref->components r key)]
               (recur (next exprs)
                 (reduce #(add-focused-query key tgt %1 %2)
                   (cond-> tx'
                     (empty? cs) (conj expr)) cs)))
             (recur (next exprs) (conj tx' expr))))
         tx')))
    tx))

(defn set-query!
  "Change the query of a component. Takes a map containing :params and/or
   :query. :params should be a map of new bindings and :query should be a query
   expression. Will schedule a re-render as well as remote re-sends if
   necessary."
  ([x params&query]
    (set-query! x params&query nil))
  ([x {:keys [params query]} reads]
   {:pre [(or (reconciler? x)
              (component? x))
          (or (not (nil? params))
              (not (nil? query)))
          (or (nil? reads)
              (vector? reads))]}
   (let [r    (if (component? x)
                (get-reconciler x)
                x)
         c    (when (component? x) x)
         root (:root @(:state r))
         cfg  (:config r)
         st   (:state cfg)
         id   (random-uuid)
         _    (.add (:history cfg) id @st)]
     (when-let [l (:logger cfg)]
       (glog/info l
         (str (when-let [ident (when (implements? Ident c)
                                 (ident c (props c)))]
                (str (pr-str ident) " "))
           (when (reconciler? x) "reconciler ")
           (when query "changed query '" query ", ")
           (when params "changed params " params " ")
           (pr-str id))))
     (swap! st update-in [:om.next/queries (or c root)] merge
       (merge (when query {:query query}) (when params {:params params})))
     (when (and (not (nil? c)) (nil? reads))
       (p/queue! r [c]))
     (when-not (nil? reads)
       (p/queue! r reads))
     (p/reindex! r)
     (let [rootq (if (not (nil? c))
                   (full-query c)
                   (when (nil? reads)
                     (get-query root)))
           sends (gather-sends (to-env cfg)
                   (into (or rootq []) (transform-reads r reads)) (:remotes cfg))]
       (when-not (empty? sends)
         (p/queue-sends! r sends)
         (schedule-sends! r)))
     nil)))

(defn update-query!
  "Update a component's query and query parameters with a function."
  ([component f]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)})))
  ([component f arg0]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0)))
  ([component f arg0 arg1]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0 arg1)))
  ([component f arg0 arg1 arg2]
   (set-query! component
     (f {:query  (get-unbound-query component)
         :params (get-params component)}
       arg0 arg1 arg2)))
  ([component f arg0 arg1 arg2 arg3 & arg-rest]
   (set-query! component
     (apply f {:query  (get-unbound-query component)
               :params (get-params component)}
       arg0 arg1 arg2 arg3 arg-rest))))

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

(defn- raw-class-path [c]
  "Return the raw component class path associated with a component. Contains
   duplicates for recursive component trees."
  (loop [c c ret (list (type c))]
    (if-let [p (parent c)]
      (if (iquery? p)
        (recur p (cons (type p) ret))
        (recur p ret))
      ret)))

(defn class-path [c]
  "Return the component class path associated with a component."
  {:pre [(component? c)]}
  (let [raw-cp (raw-class-path c)]
    (loop [cp (seq raw-cp) ret [] seen #{}]
      (if cp
        (let [c (first cp)]
          (if (contains? seen c)
            (recur (next cp) ret seen)
            (recur (next cp) (conj ret c) (conj seen c))))
        (seq ret)))))

(defn- ^boolean recursive-class-path? [c]
  "Returns true if a component's classpath is recursive"
  {:pre [(component? c)]}
  (not (apply distinct? (raw-class-path c))))

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

(defn get-ident [x]
  "Given a mounted component with assigned props, return the ident for the
   component."
  {:pre [(component? x)]}
  (let [m (props x)]
    (assert (not (nil? m)) "get-ident invoked on component with nil props")
    (ident x m)))

;; =============================================================================
;; Reconciler API

(declare reconciler?)

(defn- basis-t [reconciler]
  (p/basis-t reconciler))

(defn- queue-render! [f]
  (cond
    (fn? *raf*) (*raf* f)

    (not (exists? js/requestAnimationFrame))
    (js/setTimeout f 16)

    :else
    (js/requestAnimationFrame f)))

(defn schedule-render! [reconciler]
  (when (p/schedule-render! reconciler)
    (queue-render! #(p/reconcile! reconciler))))

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
      (schedule-sends! r))
    v))

(defn annotate-mutations
  "Given a query expression annotate all mutations by adding a :mutator -> ident
   entry to the metadata of each mutation expression in the query."
  [tx ident]
  (letfn [(annotate [expr ident]
            (cond-> expr
              (util/mutation? expr) (vary-meta assoc :mutator ident)))]
    (into [] (map #(annotate % ident)) tx)))

(defn some-iquery? [c]
  (loop [c c]
    (cond
      (nil? c) false
      (iquery? c) true
      :else (recur (parent c)))))

(defn transact!
  "Given a reconciler or component run a transaction. tx is a parse expression
   that should include mutations followed by any necessary read. The reads will
   be used to trigger component re-rendering.

   Example:

     (om/transact! widget
       '[(do/this!) (do/that!)
         :read/this :read/that])"
  ([x tx]
   {:pre [(or (component? x)
              (reconciler? x))
          (vector? tx)]}
   (let [tx (cond-> tx
              (and (component? x) (satisfies? Ident x))
              (annotate-mutations (get-ident x)))]
     (cond
       (reconciler? x) (transact* x nil nil tx)
       (not (iquery? x)) (do
                           (invariant (some-iquery? x)
                             (str "transact! should be called on a component"
                                  "that implements IQuery or has a parent that"
                                  "implements IQuery"))
                           (transact* (get-reconciler x) nil nil tx))
       :else (do
               (loop [p (parent x) x x tx tx]
                 (if (nil? p)
                   (let [r (get-reconciler x)]
                     (transact* r x nil (transform-reads r tx)))
                   (let [[x' tx] (if (implements? ITxIntercept p)
                                   [p (tx-intercept p tx)]
                                   [x tx])]
                     (recur (parent p) x' tx))))))))
  ([r ref tx]
   (transact* r nil ref tx)))

;; =============================================================================
;; Touch

(defn touch* [r c tx]
  (let [cfg  (:config r)
        env  (merge
               (to-env cfg)
               {:reconciler r :component c})
        _    (when-let [l (:logger cfg)]
               (glog/info l (str "touched '" tx)))
        snds (gather-sends env tx (:remotes cfg))]
    (p/queue! r [c])
    (schedule-render! r)
    (when-not (empty? snds)
      (p/queue-sends! r snds)
      (schedule-sends! r))))

(defn touch!
  "Issue a re-render on component."
  [c]
  {:pre [(component? c)]}
  (do
    (assert (iquery? c)
            (str "transact! invoked by component " c
                 " that does not implement IQuery"))
    (loop [p (parent c) c c tx (full-query c)]
      (if (nil? p)
        (let [r (get-reconciler c)]
          (touch* r c tx))
        (let [[x' tx] (if (implements? ITxIntercept p)
                        [p (tx-intercept p tx)]
                        [c tx])]
          (recur (parent p) x' tx))))))

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

(defn- cascade-query
  "Cascades a query up the classpath. class-path->query is a map of classpaths
   to their queries. classpath is the classpath where we start cascading (typically
   the direct parent's classpath of the component changing query). data-path is
   the data path in the classpath's query to the new query. new-query is the
   query to be applied to the classpaths. union-keys are any keys into union
   queries found during index building; they are used to access union queries
   when cascading the classpath, and to generate the classpaths' rendered-paths,
   which skip these keys."
  [class-path->query classpath data-path new-query union-keys]
  (loop [cp classpath
         data-path data-path
         new-query new-query
         ret {}]
    (if-not (empty? cp)
      (let [rendered-data-path (into [] (remove (set union-keys)) data-path)
            filter-data-path (cond-> rendered-data-path
                               (not (empty? rendered-data-path)) pop)
            qs (filter #(= filter-data-path
                           (-> % zip/root (focus->path filter-data-path)))
                 (get class-path->query cp))
            qs' (into #{}
                  (map (fn [q]
                         (let [new-query (if (or (map? (zip/node q))
                                               (some #{(peek data-path)} union-keys))
                                           (let [union-key (peek data-path)]
                                             (-> (query-template (zip/root q)
                                                   rendered-data-path)
                                                 zip/node
                                                 (assoc union-key new-query)))
                                           new-query)]
                           (-> (zip/root q)
                             (query-template rendered-data-path)
                             (replace new-query)
                             (focus-query filter-data-path)
                             (query-template filter-data-path)))))
                  qs)]
        (recur (pop cp) (pop data-path)
          (-> qs' first zip/node) (assoc ret cp qs')))
      ret)))

(defrecord Indexer [indexes extfs]
  IDeref
  (-deref [_] @indexes)

  p/IIndexer
  (index-root [_ x]
    (let [prop->classes     (atom {})
          class-path->query (atom {})
          rootq             (get-query x)
          class             (cond-> x (component? x) type)]
      (letfn [(get-dispatch-key [prop]
                (cond-> prop
                  (or (not (util/ident? prop))
                      (= (second prop) '_))
                  ((comp :dispatch-key parser/expr->ast))))
              (build-index* [class query path classpath union-expr union-keys]
                (invariant (or (not (iquery? class))
                             (and (iquery? class)
                               (not (empty? query))))
                  (str "`IQuery` implementation must return a non-empty query."
                    " Check the `IQuery` implementation of component `"
                    (if (component? class)
                      (.. class -constructor -displayName)
                      (.. class -prototype -constructor -displayName)) "`."))
                (let [recursive? (some #{class} classpath)
                      classpath  (cond-> classpath
                                   (and (not (nil? class))
                                        (not recursive?))
                                   (conj class))
                      dp->cs     (get-in @indexes [:data-path->components])]
                  (when class
                    (let [focused-query (query-template
                                          (focus-query rootq path)
                                          path)
                          cp-query      (cond-> focused-query
                                          (and (not= (zip/node focused-query) query)
                                               (not recursive?))
                                          (zip/replace query))]
                      (swap! class-path->query update-in [classpath]
                        (fnil conj #{}) cp-query)))
                  (let [recursive-join? (and recursive?
                                          (some (fn [e]
                                                  (and (util/join? e)
                                                    (not (util/recursion?
                                                           (util/join-value e)))))
                                            query)
                                          (= (distinct path) path))
                        recursive-union? (and recursive?
                                           union-expr
                                           (= (distinct path) path))]
                    (when (or (not recursive?)
                            recursive-join?
                            recursive-union?)
                      (cond
                        (vector? query)
                        (let [{props false joins true} (group-by util/join? query)]
                          (swap! prop->classes
                            #(merge-with into %
                               (zipmap
                                 (map get-dispatch-key props)
                                 (repeat #{class}))))
                          (doseq [join joins]
                            (let [[prop query']  (util/join-entry join)
                                  prop-dispatch-key (get-dispatch-key prop)
                                  recursion?     (util/recursion? query')
                                  union-recursion? (and recursion? union-expr)
                                  query'         (if recursion?
                                                   (if-not (nil? union-expr)
                                                     union-expr
                                                     query)
                                                   query')
                                  path'          (conj path prop)
                                  rendered-path' (into [] (remove (set union-keys) path'))
                                  cs (get dp->cs rendered-path')
                                  cascade-query? (and (= (count cs) 1)
                                                   (= (-> query' meta :component)
                                                     (type (first cs)))
                                                   (not (map? query')))
                                  query''        (if cascade-query?
                                                   (get-query (first cs))
                                                   query')]
                              (swap! prop->classes
                                #(merge-with into % {prop-dispatch-key #{class}}))
                              (when (and cascade-query? (not= query' query''))
                                (let [cp->q' (cascade-query @class-path->query classpath
                                               path' query'' union-keys)]
                                  (swap! class-path->query merge cp->q')))
                              (let [class' (-> query'' meta :component)]
                                (when-not (and recursion? (nil? class'))
                                  (build-index* class' query''
                                    path' classpath (if recursion? union-expr nil) union-keys))))))

                        ;; Union query case
                        (map? query)
                        (doseq [[prop query'] query]
                          (let [path'          (conj path prop)
                                class'         (-> query' meta :component)
                                cs             (filter #(= class' (type %))
                                                 (get dp->cs path))
                                cascade-query? (and class' (= (count cs) 1))
                                query''        (if cascade-query?
                                                 (get-query (first cs))
                                                 query')]
                            (when (and cascade-query? (not= query' query''))
                              (let [qs        (get @class-path->query classpath)
                                    q         (first qs)
                                    qnode     (zip/node
                                                (cond-> q
                                                  (nil? class) (query-template path)))
                                    new-query (assoc qnode
                                                prop query'')
                                    q'        (cond-> (zip/replace
                                                        (query-template (zip/root q) path)
                                                        new-query)
                                                (nil? class)
                                                (-> zip/root
                                                  (focus-query (pop path))
                                                  (query-template (pop path))))
                                    qs'       (into #{q'} (remove #{q}) qs)
                                    cp->q'    (merge {classpath qs'}
                                                (cascade-query @class-path->query
                                                  (pop classpath) path
                                                  (zip/node q') union-keys))]
                                (swap! class-path->query merge cp->q')))
                            (build-index* class' query'' path' classpath query (conj union-keys prop)))))))))]
        (build-index* class rootq [] [] nil [])
        (swap! indexes merge
          {:prop->classes     @prop->classes
           :class-path->query @class-path->query}))))

  (index-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in ((:index-component extfs) indexes c)
                        [:class->components (type c)]
                        (fnil conj #{}) c)
              data-path (into [] (remove number?) (path c))
              indexes (update-in ((:index-component extfs) indexes c)
                        [:data-path->components data-path]
                        (fnil conj #{}) c)
              ident     (when (implements? Ident c)
                          (let [ident (ident c (props c))]
                            (invariant (util/ident? ident)
                              (str "malformed Ident. An ident must be a vector of "
                                "two elements (a keyword and an EDN value). Check "
                                "the Ident implementation of component `"
                                (.. c -constructor -displayName) "`."))
                            ident))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] (fnil conj #{}) c))
            indexes)))))

  (drop-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in ((:drop-component extfs) indexes c)
                        [:class->components (type c)]
                        disj c)
              data-path (into [] (remove number?) (path c))
              indexes (update-in ((:drop-component extfs) indexes c)
                        [:data-path->components data-path]
                        disj c)
              ident     (when (implements? Ident c)
                        (ident c (props c)))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] disj c))
            indexes)))))

  (key->components [_ k]
    (let [indexes @indexes]
      (if (component? k)
        #{k}
        (if-let [cs ((:ref->components extfs) indexes k)]
          cs
          (let [cs (get-in indexes [:ref->components k] ::not-found)]
            (if-not (keyword-identical? ::not-found cs)
              cs
              (if (keyword? k)
                ;; TODO: more robust validation, might be bogus key
                (let [cs (get-in indexes [:prop->classes k])]
                  (transduce (map #(get-in indexes [:class->components %]))
                    (completing into) #{} cs))
                #{}))))))))

(defn indexer
  "Given a function (Component -> Ref), return an indexer."
  ([]
    (indexer
      {:index-component (fn [indexes component] indexes)
       :drop-component  (fn [indexes component] indexes)
       :ref->components (fn [indexes ref] nil)}))
  ([extfs]
   (Indexer.
     (atom
       {:class->components {}
        :data-path->components {}
        :ref->components   {}})
     extfs)))

(defn get-indexer
  "PRIVATE: Get the indexer associated with the reconciler."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :indexer))

(defn ref->components
  "Return all components for a given ref."
  [x ref]
  (when-not (nil? ref)
    (let [indexer (if (reconciler? x) (get-indexer x) x)]
      (p/key->components indexer ref))))

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

(defn class-path->queries
  "Given x (a reconciler or indexer) and y (a component or component class
   path), return the queries for that path."
  [x y]
  (let [indexer (if (reconciler? x) (get-indexer x) x)
        cp      (if (component? y) (class-path y) y)]
    (into #{} (map zip/root)
      (get-in @indexer [:class-path->query cp]))))

(defn full-query
  "Returns the absolute query for a given component, not relative like
   om.next/get-query."
  ([component]
   (when (iquery? component)
     (if (nil? (path component))
       (replace
         (first
           (get-in @(-> component get-reconciler get-indexer)
             [:class-path->query (class-path component)]))
         (get-query component))
       (full-query component (get-query component)))))
  ([component query]
   (when (iquery? component)
     (let [xf    (cond->> (remove number?)
                   (recursive-class-path? component) (comp (distinct)))
           path' (into [] xf (path component))
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

(defn- normalize* [query data refs union-seen]
  (cond
    (= '[*] query) data

    ;; union case
    (map? query)
    (let [class (-> query meta :component)
          ident   (when (implements? Ident class)
                  (ident class data))]
      (if-not (nil? ident)
        (vary-meta (normalize* (get query (first ident)) data refs union-seen)
          assoc :om/tag (first ident))
        (throw (js/Error. "Union components must implement Ident"))))

    (vector? data) data ;; already normalized

    :else
    (loop [q (seq query) ret data]
      (if-not (nil? q)
        (let [expr (first q)]
          (if (util/join? expr)
            (let [[k sel] (util/join-entry expr)
                  recursive? (util/recursion? sel)
                  union-entry (if (util/union? expr) sel union-seen)
                  sel     (if recursive?
                            (if-not (nil? union-seen)
                              union-seen
                              query)
                            sel)
                  class   (-> sel meta :component)
                  v       (get data k)]
              (cond
                ;; graph loop: db->tree leaves ident in place
                (and recursive? (util/ident? v)) (recur (next q) ret)
                ;; normalize one
                (map? v)
                (let [x (normalize* sel v refs union-entry)]
                  (if-not (or (nil? class) (not (implements? Ident class)))
                    (let [i (ident class v)]
                      (swap! refs update-in [(first i) (second i)] merge x)
                      (recur (next q) (assoc ret k i)))
                    (recur (next q) (assoc ret k x))))

                ;; normalize many
                (vector? v)
                (let [xs (into [] (map #(normalize* sel % refs union-entry)) v)]
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
            (let [k (if (seq? expr) (first expr) expr)
                  v (get data k)]
              (if (nil? v)
                (recur (next q) ret)
                (recur (next q) (assoc ret k v))))))
        ret))))

(defn tree->db
  "Given a Om component class or instance and a tree of data, use the component's
   query to transform the tree into the default database format. All nodes that
   can be mapped via Ident implementations wil be replaced with ident links. The
   original node data will be moved into tables indexed by ident. If merge-idents
   option is true, will return these tables in the result instead of as metadata."
  ([x data]
    (tree->db x data false))
  ([x data ^boolean merge-idents]
   (let [refs (atom {})
         x    (if (vector? x) x (get-query x))
         ret  (normalize* x data refs nil)]
     (if merge-idents
       (let [refs' @refs]
         (assoc (merge ret refs')
           ::tables (into #{} (keys refs'))))
       (with-meta ret @refs)))))

(defn- sift-idents [res]
  (let [{idents true rest false} (group-by #(vector? (first %)) res)]
    [(into {} idents) (into {} rest)]))

(defn reduce-query-depth
  "Changes a join on key k with depth limit from [:a {:k n}] to [:a {:k (dec n)}]"
  [q k]
  (if-not (empty? (focus-query q [k]))
    (let [pos (query-template q [k])
          node (zip/node pos)
          node' (cond-> node (number? node) dec)]
      (replace pos node'))
    q))

(defn- reduce-union-recursion-depth
  "Given a union expression decrement each of the query roots by one if it
   is recursive."
  [union-expr recursion-key]
  (->> union-expr
    (map (fn [[k q]] [k (reduce-query-depth q recursion-key)]))
    (into {})))

(defn- mappable-ident? [refs ident]
  (and (util/ident? ident)
       (contains? refs (first ident))))

;; TODO: easy to optimize

(defn- denormalize*
  "Denormalize a data based on query. refs is a data structure which maps idents
   to their values. map-ident is a function taking a ident to another ident,
   used during tempid transition. idents-seen is the set of idents encountered,
   used to limit recursion. union-expr is the current union expression being
   evaluated. recurse-key is key representing the current recursive query being
   evaluted."
  [query data refs map-ident idents-seen union-expr recurse-key]
  ;; support taking ident for data param
  (let [union-recur? (and union-expr recurse-key)
        recur-ident (when union-recur?
                      data)
        data (loop [data data]
               (if (mappable-ident? refs data)
                 (recur (get-in refs (map-ident data)))
                 data))]
    (cond
      (vector? data)
      ;; join
      (let [step (fn [ident]
                   (if-not (mappable-ident? refs ident)
                     (if (= query '[*])
                       ident
                       (let [{props false joins true} (group-by util/join? query)
                             props (mapv #(cond-> % (seq? %) first) props)]
                         (loop [joins (seq joins) ret {}]
                           (if-not (nil? joins)
                             (let [join        (first joins)
                                   [key sel]   (util/join-entry join)
                                   v           (get ident key)]
                               (recur (next joins)
                                 (assoc ret
                                   key (denormalize* sel v refs map-ident
                                         idents-seen union-expr recurse-key))))
                             (merge (select-keys ident props) ret)))))
                     (let [ident'       (get-in refs (map-ident ident))
                           query        (cond-> query
                                          union-recur? (reduce-union-recursion-depth recurse-key))
                           ;; also reduce query depth of union-seen, there can
                           ;; be more union recursions inside
                           union-seen'  (cond-> union-expr
                                          union-recur? (reduce-union-recursion-depth recurse-key))
                           query'       (cond-> query
                                          (map? query) (get (first ident)))] ;; UNION
                       (denormalize* query' ident' refs map-ident idents-seen union-seen' nil))))]
        (into [] (map step) data))

      (and (map? query) union-recur?)
      (denormalize* (get query (first recur-ident)) data refs map-ident
        idents-seen union-expr recurse-key)

      :else
      ;; map case
      (if (= '[*] query)
        data
        (let [{props false joins true} (group-by #(or (util/join? %)
                                                      (util/ident? %)
                                                      (and (seq? %)
                                                           (util/ident? (first %))))
                                         query)
              props (mapv #(cond-> % (seq? %) first) props)]
          (loop [joins (seq joins) ret {}]
            (if-not (nil? joins)
              (let [join        (first joins)
                    join        (cond-> join
                                  (seq? join) first)
                    join        (cond-> join
                                  (util/ident? join) (hash-map '[*]))
                    [key sel]   (util/join-entry join)
                    recurse?    (util/recursion? sel)
                    recurse-key (when recurse? key)
                    v           (if (util/ident? key)
                                  (if (= '_ (second key))
                                    (get refs (first key))
                                    (get-in refs (map-ident key)))
                                  (get data key))
                    key         (cond-> key (util/unique-ident? key) first)
                    v           (if (mappable-ident? refs v)
                                  (loop [v v]
                                    (let [next (get-in refs (map-ident v))]
                                      (if (mappable-ident? refs next)
                                        (recur next)
                                        (map-ident v))))
                                  v)
                    limit       (if (number? sel) sel :none)
                    union-entry (if (util/union? join)
                                  sel
                                  (when recurse?
                                    union-expr))
                    sel         (cond
                                  recurse?
                                  (if-not (nil? union-expr)
                                    union-entry
                                    (reduce-query-depth query key))

                                  (and (mappable-ident? refs v)
                                       (util/union? join))
                                  (get sel (first v))

                                  (and (util/ident? key)
                                       (util/union? join))
                                  (get sel (first key))

                                  :else sel)
                    graph-loop? (and recurse?
                                  (contains? (set (get idents-seen key)) v)
                                  (= :none limit))
                    idents-seen (if (and (mappable-ident? refs v) recurse?)
                                  (-> idents-seen
                                    (update-in [key] (fnil conj #{}) v)
                                    (assoc-in [:last-ident key] v)) idents-seen)]
                (cond
                  (= 0 limit) (recur (next joins) ret)
                  graph-loop? (recur (next joins) ret)
                  (nil? v)    (recur (next joins) ret)
                  :else       (recur (next joins)
                                (assoc ret
                                  key (denormalize* sel v refs map-ident
                                        idents-seen union-entry recurse-key)))))
              (if-let [looped-key (some
                                    (fn [[k identset]]
                                      (if (contains? identset (get data k))
                                        (get-in idents-seen [:last-ident k])
                                        nil))
                                    (dissoc idents-seen :last-ident))]
                looped-key
                (merge (select-keys data props) ret)))))))))

(defn db->tree
  "Given a query, some data in the default database format, and the entire
   application state in the default database format, return the tree where all
   ident links have been replaced with their original node values."
  ([query data refs]
   {:pre [(map? refs)]}
   (denormalize* query data refs identity {} nil nil))
  ([query data refs map-ident]
   {:pre [(map? refs)]}
   (denormalize* query data refs map-ident {} nil nil)))

;; =============================================================================
;; Reconciler

(defn rewrite [rewrite-map result]
  (letfn [(step [res [k orig-paths]]
            (let [to-move (get result k)
                  res'    (reduce #(assoc-in %1 (conj %2 k) to-move)
                            res orig-paths)]
              (dissoc res' k)))]
    (reduce step result rewrite-map)))

(defn- move-roots
  "When given a join `{:join selector-vector}`, roots found so far, and a `path` prefix:
  returns a (possibly empty) sequence of [re-rooted-join prefix] results.
  Does NOT support sub-roots. Each re-rooted join will share only
  one common parent (their common branching point).
  "
  [join result-roots path]
  (letfn [(query-root? [join] (true? (-> join meta :query-root)))]
    (if (util/join? join)
      (if (query-root? join)
        (conj result-roots [join path])
        (mapcat
          #(move-roots % result-roots
            (conj path (util/join-key join)))
          (util/join-value join)))
      result-roots)))

(defn- merge-joins
  "Searches a query for duplicate joins and deep-merges them into a new query."
  [query]
  (letfn [(step [res expr]
            (if (contains? (:elements-seen res) expr)
              res ; eliminate exact duplicates
              (update-in
                (if (and (util/join? expr)
                         (not (util/union? expr))
                         (not (list? expr)))
                  (let [jk (util/join-key expr)
                        jv (util/join-value expr)
                        q  (or (-> res :query-by-join (get jk)) [])
                        nq (cond
                             (util/recursion? q) q
                             (util/recursion? jv) jv
                             :else (merge-joins (into [] (concat q jv))))]
                    (update-in res [:query-by-join] assoc jk nq))
                  (update-in res [:not-mergeable] conj expr))
                [:elements-seen] conj expr)))]
    (let [init {:query-by-join {}
                :elements-seen #{}
                :not-mergeable []}
          res  (reduce step init query)]
      (->> (:query-by-join res)
        (mapv (fn [[jkey jsel]] {jkey jsel}))
        (concat (:not-mergeable res))
        (into [])))))

(defn process-roots [query]
  "A send helper for rewriting the query to remove client local keys that
   don't need server side processing. Give a query this function will
   return a map with two keys, :query and :rewrite. :query is the
   actual query you should send. Upon receiving the response you should invoke
   :rewrite on the response before invoking the send callback."
  (letfn [(retain [expr] [[expr []]]) ; emulate an alternate-root element
          (reroot [expr]
            (let [roots (move-roots expr [] [])]
              (if (empty? roots)
                (retain expr)
                roots)))
          (rewrite-map-step [rewrites [expr path]]
            (if (empty? path)
              rewrites
              (update-in rewrites [(util/join-key expr)] conj path)))]
    (let [reroots     (mapcat reroot query)
          query       (merge-joins (mapv first reroots))
          rewrite-map (reduce rewrite-map-step {} reroots)]
     {:query   query
      :rewrite (partial rewrite rewrite-map)})))

(defn- merge-idents [tree config refs query]
  (let [{:keys [merge-ident indexer]} config
        ident-joins (into {} (filter #(and (util/join? %)
                                           (util/ident? (util/join-key %)))
                               query))]
    (letfn [ (step [tree' [ident props]]
              (if (:normalize config)
                (let [c-or-q (or (get ident-joins ident) (ref->any indexer ident))
                      props' (tree->db c-or-q props)
                      refs   (meta props')]
                  ((:merge-tree config)
                    (merge-ident config tree' ident props') refs))
                (merge-ident config tree' ident props)))]
      (reduce step tree refs))))

(defn- merge-novelty!
  [reconciler state res query]
  (let [config      (:config reconciler)
        [idts res'] (sift-idents res)
        res'        (if (:normalize config)
                      (tree->db
                        (or query (:root @(:state reconciler)))
                        res' true)
                      res')]
    (-> state
      (merge-idents config idts query)
      ((:merge-tree config) res'))))

(defn default-merge [reconciler state res query]
  {:keys    (into [] (remove symbol?) (keys res))
   :next    (merge-novelty! reconciler state res query)
   :tempids (->> (filter (comp symbol? first) res)
              (map (comp :tempids second))
              (reduce merge {}))})

(defn merge!
  "Merge a state delta into the application state. Affected components managed
   by the reconciler will re-render."
  ([reconciler delta]
    (merge! reconciler delta nil))
  ([reconciler delta query]
   (let [config (:config reconciler)
         state (:state config)
         merge* (:merge config)
         {:keys [keys next tempids]} (merge* reconciler @state delta query)]
     (p/queue! reconciler keys)
     (reset! state
       (if-let [migrate (:migrate config)]
         (merge (select-keys next [:om.next/queries])
           (migrate next
             (or query (get-query (:root @(:state reconciler))))
             tempids (:id-key config)))
         next)))))

(defrecord Reconciler [config state]
  IDeref
  (-deref [this] @(:state config))

  p/IReconciler
  (basis-t [_] (:t @state))

  (add-root! [this root-class target options]
    (let [ret   (atom nil)
          rctor (factory root-class)
          guid  (random-uuid)]
      (when (iquery? root-class)
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
                                                 ((:shared-fn config) data)))
                                *instrument* (:instrument config)]
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
            (if-not (iquery? root-class)
              (queue-render! parsef)
              (schedule-render! this))))
        (parsef)
        (when-let [sel (get-query (or (and target @ret) root-class))]
          (let [env  (to-env config)
                snds (gather-sends env sel (:remotes config))]
            (when-not (empty? snds)
              (when-let [send (:send config)]
                (send snds
                  (fn [res query]
                    (merge! this res query)
                    (renderf ((:parser config) env sel))))))))
        @ret)))

  (remove-root! [_ target]
    (when-let [remove (:remove @state)]
      (remove)))

  (reindex! [this]
    (let [root (get @state :root)]
      (when (iquery? root)
        (let [indexer (:indexer config)
              c (first (get-in @indexer [:class->components root]))]
          (p/index-root indexer (or c root))))))

  (queue! [_ ks]
    (swap! state update-in [:queue] into ks))

  (queue-sends! [_ sends]
    (swap! state update-in [:queued-sends]
      (:merge-sends config) sends))

  (schedule-render! [_]
    (if-not (:queued @state)
      (do
        (swap! state assoc :queued true)
        true)
      false))

  (schedule-sends! [_]
    (if-not (:sends-queued @state)
      (do
        (swap! state assoc :sends-queued true)
        true)
      false))

  ;; TODO: need to reindex roots after reconcilation
  (reconcile! [this]
    (let [st @state
          q  (:queue st)]
      (swap! state update-in [:queued] not)
      (swap! state assoc :queue [])
      (if (empty? q)
        ;; TODO: need to move root re-render logic outside of batching logic
        ((:render st))
        (let [cs (transduce
                   (map #(p/key->components (:indexer config) %))
                   #(into %1 %2) #{} q)
              {:keys [ui->props]} config
              env (to-env config)
              root (:root @state)
              props-change? (> (p/basis-t this) (t root))]
          (doseq [c ((:optimize config) cs)]
            (when (mounted? c)
              (let [computed   (get-computed (props c))
                    next-raw-props (ui->props env c)
                    next-props     (om.next/computed next-raw-props computed)]
                (when (and (exists? (.-componentWillReceiveProps c))
                           (iquery? root)
                           props-change?)
                  ;; `componentWilReceiveProps` is always called before `shouldComponentUpdate`
                  (.componentWillReceiveProps c
                    #js {:omcljs$value (om-props next-props (p/basis-t this))}))
                (when (should-update? c next-props (get-state c))
                  (if-not (nil? next-props)
                    (update-component! c next-props)
                    (.forceUpdate c))
                  ;; Only applies if we're doing incremental rendering, not
                  ;; the case in applications without queries
                  (when (and (iquery? root)
                             (not= c root)
                             props-change?)
                    (let [update-path (path c)]
                      (loop [p (parent c)]
                        (when (some? p)
                          (let [update-path' (subvec update-path (count (path p)))]
                            (update-props! p (assoc-in (props p) update-path' next-raw-props))
                            (merge-pending-props! p)
                            (recur (parent p)))))))))))))))

  (send! [this]
    (let [sends (:queued-sends @state)]
      (when-not (empty? sends)
        (swap! state
          (fn [state]
            (-> state
              (assoc :queued-sends {})
              (assoc :sends-queued false))))
        ((:send config) sends
          (fn [res query]
            (merge! this res query)))))))

(defn default-ui->props
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

(defn default-merge-ident
  [_ tree ref props]
  (update-in tree ref merge props))

(defn default-merge-tree
  [a b]
  (if (map? a)
    (merge a b)
    b))

(defn default-migrate
  "Given app-state-pure (the application state as an immutable value), a query,
   tempids (a hash map from tempid to stable id), and an optional id-key
   keyword, return a new application state value with the tempids replaced by
   the stable ids."
  ([app-state-pure query tempids]
    (default-migrate app-state-pure query tempids nil))
  ([app-state-pure query tempids id-key]
   (letfn [(dissoc-in [pure [table id]]
             (assoc pure table (dissoc (get pure table) id)))
           (step [pure [old [_ id :as new]]]
             (-> pure
               (dissoc-in old)
               (assoc-in new
                 (cond-> (merge (get-in pure old) (get-in pure new))
                   (not (nil? id-key)) (assoc id-key id)))))]
     (if-not (empty? tempids)
       (let [pure' (reduce step app-state-pure tempids)]
         (tree->db query
           (db->tree query pure' pure'
             (fn [ident] (get tempids ident ident))) true))
       app-state-pure))))

(defn- ^boolean has-error? [x]
  (and (map? x) (contains? x ::error)))

(defn default-extract-errors [reconciler res query]
  (letfn [(extract* [query res errs]
            (let [class      (-> query meta :component)
                  top-error? (when (and (not (nil? class)) (has-error? res))
                               (swap! errs
                                 #(update-in % [(ident class res)]
                                   (fnil conj #{}) (::error res))))
                  ret        (when (nil? top-error?) {})]
              (cond
                ;; query root
                (vector? query)
                (if (vector? res)
                  (into [] (map #(extract* query % errs)) res)
                  (loop [exprs (seq query) ret ret]
                    (if-not (nil? exprs)
                      (let [expr (first exprs)
                            k    (as-> (expr->key expr) k
                                   (cond-> k
                                     (util/unique-ident? k) first))
                            data (get res k)]
                        (cond
                          (util/mutation? expr)
                          (let [mk   (util/mutation-key expr)
                                ret' (get res mk)]
                            (if (has-error? ret')
                              (let [x (-> expr meta :mutator)]
                                (swap! errs
                                  #(update-in % [x]
                                    (fnil conj #{}) (::error ret')))
                                (recur (next exprs) ret))
                              (recur (next exprs)
                                (when-not (nil? ret)
                                  (assoc ret mk ret')))))

                          (util/union? expr)
                          (let [jk     (util/join-key expr)
                                jv     (util/join-value expr)
                                class' (-> jv meta :component)]
                            (if (not (vector? data))
                              (let [ret' (extract*
                                           (get jv (first (ident class' data)))
                                           data errs)]
                                (recur (next exprs)
                                  (when-not (nil? ret)
                                    (assoc ret jk ret'))))
                              (let [ret' (into []
                                           (map #(extract* (get jv (first (ident class' %))) % errs))
                                           data)]
                                (recur (next exprs)
                                  (when-not (nil? ret)
                                    (assoc ret jk ret'))))))

                          (util/join? expr)
                          (let [jk   (util/join-key expr)
                                jv   (util/join-value expr)
                                ret' (extract* jv data errs)]
                            (recur (next exprs)
                              (when-not (nil? ret)
                                (assoc ret jk ret'))))

                          (and (map? data) (has-error? data))
                          (do
                            (swap! errs
                              #(update-in %
                                [(or (when-not (nil? class) (ident class res)) k)]
                                (fnil conj #{}) (::error data)))
                            (recur (next exprs) nil))

                          :else
                          (recur (next exprs)
                            (when-not (nil? ret)
                              (assoc ret k data)))))
                      ret))))))]
    (let [errs (atom {})
          ret  (extract* query res errs)]
      {:tree ret :errors @errs})))

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
                     loading if desired. The callback should take the response as the
                     first argument and the the query that was sent as the second
                     argument.
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
           merge merge-tree merge-ident
           prune-tree
           optimize
           history
           root-render root-unmount
           pathopt
           migrate id-key
           instrument
           easy-reads]
    :or {ui->props    default-ui->props
         indexer      om.next/indexer
         merge-sends  #(merge-with into %1 %2)
         remotes      [:remote]
         merge        default-merge
         merge-tree   default-merge-tree
         merge-ident  default-merge-ident
         prune-tree   default-extract-errors
         optimize     (fn [cs] (sort-by depth cs))
         history      100
         root-render  #(js/ReactDOM.render %1 %2)
         root-unmount #(js/ReactDOM.unmountComponentAtNode %)
         pathopt      false
         migrate      default-migrate
         easy-reads   true}
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
                  :merge merge :merge-tree merge-tree :merge-ident merge-ident
                  :prune-tree prune-tree
                  :optimize optimize
                  :normalize (or (not norm?) normalize)
                  :history (c/cache history)
                  :root-render root-render :root-unmount root-unmount
                  :logger logger :pathopt pathopt
                  :migrate migrate :id-key id-key
                  :instrument instrument
                  :easy-reads easy-reads}
                 (atom {:queue [] :queued false :queued-sends {}
                        :sends-queued false
                        :target nil :root nil :render nil :remove nil
                        :t 0 :normalized norm?}))]
    ret))

(defn ^boolean reconciler?
  "Returns true if x is a reconciler."
  [x]
  (instance? Reconciler x))

(defn app-state
  "Return the reconciler's application state atom. Useful when the reconciler
   was initialized via denormalized data."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :state))

(defn app-root
  "Return the application's root component."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (get @(:state reconciler) :root))

(defn force-root-render!
  "Force a re-render of the root. Not recommended for anything except
   recomputing :shared."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  ((get @(:state reconciler) :render)))

(defn from-history
  "Given a reconciler and UUID return the application state snapshost
   from history associated with the UUID. The size of the reconciler history
   may be configured by the :history option when constructing the reconciler."
  [reconciler uuid]
  {:pre [(reconciler? reconciler)]}
  (.get (-> reconciler :config :history) uuid))

(defn tempid
  "Return a temporary id."
  ([] (tempid/tempid))
  ([id] (tempid/tempid id)))

(defn ^boolean tempid?
  "Return true if x is a tempid, false otherwise"
  [x]
  (tempid/tempid? x))

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
