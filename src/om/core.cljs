(ns om.core
  (:require-macros [om.core :refer [check allow-reads]])
  (:require [om.dom :as dom :include-macros true])
  (:import [goog.ui IdGenerator]))

(def ^{:tag boolean :dynamic true :private true} *read-enabled* false)
(def ^{:dynamic true :private true} *parent* nil)
(def ^{:dynamic true :private true} *instrument* nil)
(def ^{:dynamic true :private true} *state* nil)

;; =============================================================================
;; React Life Cycle Protocols
;;
;; http://facebook.github.io/react/docs/component-specs.html

(defprotocol IDisplayName
  (display-name [this]))

(defprotocol IInitState
  (init-state [this]))

(defprotocol IShouldUpdate
  (should-update [this next-props next-state]))

(defprotocol IWillMount
  (will-mount [this]))

(defprotocol IDidMount
  (did-mount [this]))

(defprotocol IWillUnmount
  (will-unmount [this]))

(defprotocol IWillUpdate
  (will-update [this next-props next-state]))

(defprotocol IDidUpdate
  (did-update [this prev-props prev-state]))

(defprotocol IWillReceiveProps
  (will-receive-props [this next-props]))

(defprotocol IRender
  (render [this]))

(defprotocol IRenderState
  (render-state [this state]))

;; =============================================================================
;; Om Protocols

(defprotocol IOmSwap
  (-om-swap! [this cursor korks f tag]))

(defprotocol IGetState
  (-get-state [this] [this ks]))

(defprotocol IGetRenderState
  (-get-render-state [this] [this ks]))

(defprotocol ISetState
  (-set-state! [this val render] [this ks val render]))

;; private render queue, for components that use local state
;; and independently addressable components

(defprotocol IRenderQueue
  (-get-queue [this])
  (-queue-render! [this c])
  (-empty-queue! [this]))

(defprotocol IValue
  (-value [x]))

(extend-type default
  IValue
  (-value [x] x))

(defprotocol ICursor
  (-path [cursor])
  (-state [cursor]))

(defprotocol IToCursor
  (-to-cursor [value state] [value state path]))

(defprotocol ICursorDerive
  (-derive [cursor derived state path]))

(declare to-cursor)

(extend-type default
  ICursorDerive
  (-derive [this derived state path]
    (to-cursor derived state path)))

(defn path [cursor]
  (-path cursor))

(defn value [cursor]
  (-value cursor))

(defn state [cursor]
  (-state cursor))

(defprotocol ITransact
  (-transact! [cursor korks f tag]))

(defprotocol INotify
  (-listen! [x key tx-listen])
  (-unlisten! [x key])
  (-notify! [x tx-data root-cursor]))

(declare notify* path)

(defn transact*
  ([state cursor korks f tag]
     (let [old-state @state
           path (into (om.core/path cursor) korks)
           ret (cond
                 (satisfies? IOmSwap state) (-om-swap! state cursor korks f tag)
                 (empty? path) (swap! state f)
                 :else (swap! state update-in path f))]
       (when-not (= ret ::defer)
         (let [tx-data {:path path
                        :old-value (get-in old-state path)
                        :new-value (get-in @state path)
                        :old-state old-state
                        :new-state @state}]
           (if-not (nil? tag)
             (notify* cursor (assoc tx-data :tag tag))
             (notify* cursor tx-data)))))))

(defn cursor? [x]
  (satisfies? ICursor x))

(defn ^:private children [node]
  (let [c (.. node -props -children)]
    (if (fn? c)
      (set! (.. node -props -children) (allow-reads (c node)))
      c)))

(defn get-props
  "Given an owning Pure node return the Om props. Analogous to React
   component props."
  [x]
  (aget (.-props x) "__om_cursor"))

(defn get-state
  "Returns the component local state of an owning component. owner is
   the component. An optional key or sequence of keys may be given to
   extract a specific value. Always returns pending state."
  ([owner]
     (-get-state owner))
  ([owner korks]
     (let [ks (if (sequential? korks) korks [korks])] 
       (-get-state owner ks))))

(defn get-shared
  "Takes an owner and returns a map of global shared values for a
   render loop. An optional key or sequence of keys may be given to
   extract a specific value."
  ([owner]
    (when-not (nil? owner)
      (aget (.-props owner) "__om_shared")))
  ([owner korks]
    (cond
      (not (sequential? korks))
      (get (get-shared owner) korks)

      (empty? korks)
      (get-shared owner)

      :else
      (get-in (get-shared owner) korks))))

(defn ^:private merge-pending-state [owner]
  (let [state (.-state owner)]
    (when-let [pending-state (aget state "__om_pending_state")]
      (doto state
        (aset "__om_prev_state" (aget state "__om_state"))
        (aset "__om_state" pending-state)
        (aset "__om_pending_state" nil)))))

(defn ^:private merge-props-state
  ([owner] (merge-props-state owner nil))
  ([owner props]
    (let [props (or props (.-props owner))]
      (when-let [props-state (aget props "__om_state")]
        (let [state (.-state owner)]
          (aset state "__om_pending_state"
               (merge (or (aget state "__om_pending_state")
                          (aget state "__om_state"))
                      props-state))
          (aset props "__om_state" nil))))))

(def pure-methods
  {:getDisplayName
   (fn []
     (this-as this
       (let [c (children this)]
         (when (satisfies? IDisplayName c)
           (allow-reads (display-name c))))))
   :getInitialState
   (fn []
     (this-as this
       (let [c      (children this)
             props  (.-props this)
             istate (or (aget props "__om_init_state") {})
             id     (::id istate)
             ret    #js {:__om_id (or id (.getNextUniqueId (.getInstance IdGenerator)))
                         :__om_state
                         (merge
                           (when (satisfies? IInitState c)
                             (allow-reads (init-state c)))
                           (dissoc istate ::id))}]
         (aset props "__om_init_state" nil)
         ret)))
   :shouldComponentUpdate
   (fn [next-props next-state]
     (this-as this
       (allow-reads
         (let [props (.-props this)
               state (.-state this)
               c     (children this)]
           ;; need to merge in props state first
           (merge-props-state this next-props)
           (if (satisfies? IShouldUpdate c)
             (should-update c
               (get-props #js {:props next-props})
               (-get-state this))
             (cond
               (not= (-value (aget props "__om_cursor"))
                     (-value (aget next-props "__om_cursor")))
               true

               (not (nil? (aget state "__om_pending_state")))
               true

               (not (== (aget props "__om_index") (aget next-props "__om_index")))
               true

               :else false))))))
   :componentWillMount
   (fn []
     (this-as this
       (merge-props-state this)
       (let [c (children this)]
         (when (satisfies? IWillMount c)
           (allow-reads (will-mount c))))
       (merge-pending-state this)))
   :componentDidMount
   (fn []
     (this-as this
       (let [c (children this)]
         (when (satisfies? IDidMount c)
           (allow-reads (did-mount c))))))
   :componentWillUnmount
   (fn []
     (this-as this
       (let [c (children this)]
         (when (satisfies? IWillUnmount c)
           (allow-reads (will-unmount c))))))
   :componentWillUpdate
   (fn [next-props next-state]
     (this-as this
       (let [c (children this)]
         (when (satisfies? IWillUpdate c)
           (let [state (.-state this)]
             (allow-reads
               (will-update c
                 (get-props #js {:props next-props})
                 (-get-state this))))))
       (merge-pending-state this)))
   :componentDidUpdate
   (fn [prev-props prev-state]
     (this-as this
       (let [c (children this)]
         (when (satisfies? IDidUpdate c)
           (let [state (.-state this)]
             (allow-reads
               (did-update c
                 (get-props #js {:props prev-props})
                 (or (aget state "__om_prev_state")
                     (aget state "__om_state"))))))
         (aset (.-state this) "__om_prev_state" nil))))
   :componentWillReceiveProps
   (fn [next-props]
     (this-as this
       (let [c (children this)]
         (when (satisfies? IWillReceiveProps c)
           (allow-reads
             (will-receive-props c
               (get-props #js {:props next-props})))))))
   :render
   (fn []
     (this-as this
       (let [c (children this)
             props (.-props this)]
         (allow-reads
           (cond
             (satisfies? IRender c)
             (binding [*parent*     this
                       *state*      (aget props "__om_app_state")
                       *instrument* (aget props "__om_instrument")]
               (render c))

             (satisfies? IRenderState c)
             (binding [*parent*     this
                       *state*      (aget props "__om_app_state")
                       *instrument* (aget props "__om_instrument")]
               (render-state c (get-state this)))
             :else c)))))})

(defn specify-state-methods! [obj]
  (specify! obj
    ISetState
    (-set-state!
      ([this val render]
         (allow-reads
           (let [props     (.-props this)
                 app-state (aget props "__om_app_state")]
             (aset (.-state this) "__om_pending_state" val)
             (when (and (not (nil? app-state)) render)
               (-queue-render! app-state this)))))
      ([this ks val render]
         (allow-reads
           (let [props     (.-props this)
                 state     (.-state this)
                 pstate    (-get-state this)
                 app-state (aget props "__om_app_state")]
             (aset state "__om_pending_state" (assoc-in pstate ks val))
             (when (and (not (nil? app-state)) render)
               (-queue-render! app-state this))))))
    IGetRenderState
    (-get-render-state
      ([this]
         (aget (.-state this) "__om_state"))
      ([this ks]
         (get-in (-get-render-state this) ks)))
    IGetState
    (-get-state
      ([this]
         (let [state (.-state this)]
           (or (aget state "__om_pending_state")
             (aget state "__om_state"))))
      ([this ks]
         (get-in (-get-state this) ks)))))

(def pure-descriptor
  (specify-state-methods! (clj->js pure-methods)))

;; =============================================================================
;; Cursors

(deftype MapCursor [value state path]
  IWithMeta
  (-with-meta [_ new-meta]
    (check
      (MapCursor. (with-meta value new-meta) state path)))
  IMeta
  (-meta [_] (check (meta value)))
  IDeref
  (-deref [this]
    (if-not *read-enabled*
      (get-in @state path)
      (throw (js/Error. (str "Cannot deref cursor during render phase: " this)))))
  IValue
  (-value [_] value)
  ICursor
  (-path [_] path)
  (-state [_] state)
  ITransact
  (-transact! [this korks f tag]
    (transact* state this korks f tag))
  ICloneable
  (-clone [_]
    (MapCursor. value state path))
  ICounted
  (-count [_]
    (check (-count value)))
  ICollection
  (-conj [_ o]
    (check (MapCursor. (-conj value o) state path)))
  ;; EXPERIMENTAL
  IEmptyableCollection
  (-empty [_]
    (check
      (MapCursor. (empty value) state path)))
  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [this k not-found]
    (check
      (let [v (-lookup value k not-found)]
        (if-not (= v not-found)
          (-derive this v state (conj path k))
          not-found))))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  ISeqable
  (-seq [this]
    (check
      (when (pos? (count value))
        (map (fn [[k v]] [k (-derive this v state (conj path k))]) value))))
  IAssociative
  (-contains-key? [_ k]
    (check (-contains-key? value k)))
  (-assoc [_ k v]
    (check (MapCursor. (-assoc value k v) state path)))
  IMap
  (-dissoc [_ k]
    (check (MapCursor. (-dissoc value k) state path)))
  IEquiv
  (-equiv [_ other]
    (check
      (if (cursor? other)
        (= value (-value other))
        (= value other))))
  IHash
  (-hash [_]
    (check (hash value)))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (check (-pr-writer value writer opts))))

(deftype IndexedCursor [value state path]
  ISequential
  IDeref
  (-deref [this]
    (if-not *read-enabled*
      (get-in @state path)
      (throw (js/Error. (str "Cannot deref cursor during render phase: " this)))))
  IWithMeta
  (-with-meta [_ new-meta]
    (check
      (IndexedCursor. (with-meta value new-meta) state path)))
  IMeta
  (-meta [_] (check (meta value)))
  IValue
  (-value [_] value)
  ICursor
  (-path [_] path)
  (-state [_] state)
  ITransact
  (-transact! [this korks f tag]
    (transact* state this korks f tag))
  ICloneable
  (-clone [_]
    (IndexedCursor. value state path))
  ICounted
  (-count [_]
    (check (-count value)))
  ICollection
  (-conj [_ o]
    (check (IndexedCursor. (-conj value o) state path)))
  ;; EXPERIMENTAL
  IEmptyableCollection
  (-empty [_]
    (check (IndexedCursor. (empty value) state path)))
  ILookup
  (-lookup [this n]
    (check (-nth this n nil)))
  (-lookup [this n not-found]
    (check (-nth this n not-found)))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  IIndexed
  (-nth [this n]
    (check (-derive this (-nth value n) state (conj path n))))
  (-nth [this n not-found]
    (check
      (if (< n (-count value))
        (-derive this (-nth value n) state (conj path n))
        not-found)))
  ISeqable
  (-seq [this]
    (check
      (when (pos? (count value))
        (map (fn [v i] (-derive this v state (conj path i))) value (range)))))
  IAssociative
  (-contains-key? [_ k]
    (check (-contains-key? value k)))
  (-assoc [this n v]
    (check (-derive this (-assoc-n value n v) state path)))
  IStack
  (-peek [this]
    (check (-derive this (-peek value) state path)))
  (-pop [this]
    (check (-derive this (-pop value) state path)))
  IEquiv
  (-equiv [_ other]
    (check
      (if (cursor? other)
        (= value (-value other))
        (= value other))))
  IHash
  (-hash [_]
    (check (hash value)))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (check (-pr-writer value writer opts))))

(defn ^:private to-cursor* [val state path]
  (specify val
    IDeref
    (-deref [this]
      (if-not *read-enabled*
        (get-in @state path)
        (throw (js/Error. (str "Cannot deref cursor during render phase: " this)))))
    ICursor
    (-path [_] path)
    (-state [_] state)
    ITransact
    (-transact! [this korks f tag]
      (transact* state this korks f tag))
    IEquiv
    (-equiv [_ other]
      (check
        (if (cursor? other)
          (= val (-value other))
          (= val other))))))

(defn ^:private to-cursor
  ([val] (to-cursor val nil []))
  ([val state] (to-cursor val state []))
  ([val state path]
    (cond
      (cursor? val) val
      (satisfies? IToCursor val) (-to-cursor val state path)
      (indexed? val) (IndexedCursor. val state path)
      (map? val) (MapCursor. val state path)
      (satisfies? ICloneable val) (to-cursor* val state path)
      :else val)))

(defn notify* [cursor tx-data]
  (let [state (-state cursor)]
    (-notify! state tx-data (to-cursor @state state))))

;; =============================================================================
;; API

(def ^:private refresh-queued false)
(def ^:private refresh-set (atom #{}))

(defn ^:private render-all []
  (set! refresh-queued false)
  (doseq [f @refresh-set] (f)))

(def ^:private roots (atom {}))

(defn ^:private valid-component? [x f]
  (assert
    (or (satisfies? IRender x)
        (satisfies? IRenderState x))
    (str "Invalid Om component fn, " (.-name f)
         " does not return valid instance")))

(defn ^:private valid? [m]
  (every? #{:key :react-key :fn :init-state :state
            :opts :shared ::index :instrument :descriptor}
    (keys m)))

(defn id [owner]
  (aget (.-state owner) "__om_id"))

(defn get-descriptor
  ([f] (get-descriptor f nil))
  ([f descriptor]
     (when (nil? (aget f "om$descriptor"))
       (aset f "om$descriptor"
         (js/React.createClass (or descriptor pure-descriptor))))
     (aget f "om$descriptor")))

(defn build*
  ([f cursor] (build* f cursor nil))
  ([f cursor m]
     (assert (valid? m)
       (apply str "build options contains invalid keys, only :key, :react-key, "
         ":fn, :init-state, :state, and :opts allowed, given "
         (interpose ", " (keys m))))
     (cond
       (nil? m)
       (let [shared (get-shared *parent*)
             ctor   (get-descriptor f)]
         (ctor #js {:__om_cursor cursor
                    :__om_shared shared
                    :__om_app_state *state*
                    :__om_instrument *instrument*
                    :children
            (fn [this]
              (allow-reads
                (let [ret (f cursor this)]
                  (valid-component? ret f)
                  ret)))}))

       :else
       (let [{:keys [key state init-state opts]} m
             dataf   (get m :fn)
             cursor' (if-not (nil? dataf)
                       (if-let [i (::index m)]
                         (dataf cursor i)
                         (dataf cursor))
                       cursor)
             rkey    (if-not (nil? key)
                       (get cursor' key)
                       (get m :react-key))
             shared  (or (:shared m) (get-shared *parent*))
             ctor    (get-descriptor f (:descriptor m))]
         (ctor #js {:__om_cursor cursor'
                    :__om_index (::index m)
                    :__om_init_state init-state
                    :__om_state state
                    :__om_shared shared
                    :__om_app_state *state*
                    :__om_instrument *instrument*
                    :key rkey
                    :children
            (if (nil? opts)
              (fn [this]
                (allow-reads
                  (let [ret (f cursor' this)]
                    (valid-component? ret f)
                    ret)))
              (fn [this]
                (allow-reads
                  (let [ret (f cursor' this opts)]
                    (valid-component? ret f)
                    ret))))})))))

(defn build
  "Builds an Om component. Takes an IRender/IRenderState instance
   returning function f, a cursor, and an optional third argument
   which may be a map of build specifications.

   f - is a function of 2 or 3 arguments. The first argument will be
   the cursor and the second argument will be the owning pure node.
   If a map of options m is passed in this will be the third
   argument. f must return at a minimum an IRender or IRenderState
   instance, this instance may implement other React life cycle
   protocols.

   cursor - an ICursor instance

   m - a map the following keys are allowed:

     :key        - a keyword that should be used to look up the key used by
                   React itself when rendering sequential things.
     :react-key  - an explicit react key
     :fn         - a function to apply to the data before invoking f.
     :init-state - a map of initial state to pass to the component.
     :state      - a map of state to pass to the component, will be merged in.
     :opts       - a map of values. Can be used to pass side information down
                   the render tree.
     :descriptor - a JS object of React methods, will be used to
                   construct a React class per Om component function
                   encountered. defaults to pure-descriptor.

   Example:

     (build list-of-gadgets cursor
        {:init-state {:event-chan ...
                      :narble ...}})
  "
  ([f cursor] (build f cursor nil))
  ([f cursor m]
     (if-not (nil? *instrument*)
       (let [ret (allow-reads (*instrument* f cursor m))]
         (if (= ret ::pass)
           (build* f cursor m)
           ret))
       (build* f cursor m))))

(defn build-all
  "Build a sequence of components. f is the component constructor
   function, xs a sequence of cursors, and m a map of options the
   same as provided to om.core/build."
  ([f xs] (build-all f xs nil))
  ([f xs m]
    (map (fn [x i]
           (build f x (assoc m ::index i)))
      xs (range))))

(defn ^:private setup [state key tx-listen]
  (when-not (satisfies? INotify state)
    (let [listeners    (atom {})
          render-queue (atom #{})]
      (specify! state
        INotify
        (-listen! [this key tx-listen]
          (when-not (nil? tx-listen)
            (swap! listeners assoc key tx-listen))
          this)
        (-unlisten! [this key]
          (swap! listeners dissoc key)
          this)
        (-notify! [this tx-data root-cursor]
          (doseq [[_ f] @listeners]
            (f tx-data root-cursor))
          this)
        IRenderQueue
        (-get-queue [this] @render-queue)
        (-queue-render! [this c]
          (when-not (contains? @render-queue c)
            (swap! render-queue conj c)
            (swap! this identity)))
        (-empty-queue! [this]
          (swap! render-queue empty)))))
  (-listen! state key tx-listen))

(defn ^:private tear-down [state key]
  (-unlisten! state key))

(defn root
  "Take a component constructor function f, value an immutable tree of
   associative data structures optionally an wrapped in an IAtom
   instance, and a map of options and installs an Om/React render
   loop.

   f must return an instance that at a minimum implements IRender or
   IRenderState (it may implement other React life cycle protocols). f
   must take at least two arguments, the root cursor and the owning pure
   node. A cursor is just the original data wrapped in an ICursor
   instance which maintains path information. Only one root render
   loop allowed per target element. om.core/root is idempotent, if
   called again on the same target element the previous render loop
   will be replaced.

   Options may also include any key allowed by om.core/build to
   customize f. In addition om.core/root supports the following
   special options:

   :target     - (required) a DOM element. 
   :shared     - data to be shared by all components, see om.core/get-shared
   :tx-listen  - a function that will listen in in transactions, should
                 take 2 arguments - the first a map containing the
                 path, old and new state at path, old and new global
                 state, and transaction tag if provided.
   :instrument - a function of three arguments that if provided will
                 intercept all calls to om.core/build. This function should
                 correspond to the three arity version of om.core/build.

   Example:

   (root
     (fn [data owner]
       ...)
     {:message :hello}
     {:target js/document.body})"
  ([f value {:keys [target tx-listen path instrument] :as options}]
    (assert (not (nil? target)) "No target specified to om.core/root")
    ;; only one root render loop per target
    (let [roots' @roots]
      (when (contains? roots' target)
        ((get roots' target))))
    (let [watch-key (gensym)
          state (if (satisfies? IAtom value)
                  value
                  (atom value))
          state (setup state watch-key tx-listen)
          m     (dissoc options :target :tx-listen :path)
          rootf (fn rootf []
                  (swap! refresh-set disj rootf)
                  (let [value  @state
                        cursor (if (nil? path)
                                 (to-cursor value state [])
                                 (to-cursor (get-in value path) state path))]
                    (dom/render
                      (binding [*instrument* instrument
                                *state*      state]
                        (build f cursor m))
                      target)
                    (let [queue (-get-queue state)]
                      (when-not (empty? queue)
                        (doseq [c queue]
                          (when (.isMounted c)
                            (.forceUpdate c)))
                        (-empty-queue! state)))))]
      (add-watch state watch-key
        (fn [_ _ _ _]
          (when-not (contains? @refresh-set rootf)
            (swap! refresh-set conj rootf))
          (when-not refresh-queued
            (set! refresh-queued true)
            (if (exists? js/requestAnimationFrame)
              (js/requestAnimationFrame render-all)
              (js/setTimeout render-all 16)))))
      ;; store fn to remove previous root render loop
      (swap! roots assoc target
        (fn []
          (remove-watch state watch-key)
          (tear-down state watch-key)
          (swap! roots dissoc target)
          (js/React.unmountComponentAtNode target)))
      (rootf))))

(defn detach-root
  "Given a DOM target remove its render loop if one exists."
  [target]
  (when-let [f (get @roots target)]
    (f)))

(defn transact!
  "Given a tag, a cursor, an optional list of keys ks, mutate the tree
   at the path specified by the cursor + the optional keys by applying
   f to the specified value in the tree. An Om re-render will be
   triggered."
  ([cursor f]
    (transact! cursor [] f nil))
  ([cursor korks f]
    (transact! cursor korks f nil))
  ([cursor korks f tag]
    (let [korks (cond
                  (nil? korks) []
                  (sequential? korks) korks
                  :else [korks])]
      (-transact! cursor korks f tag))))

(defn update!
  "Like transact! but no function provided, instead a replacement
  value is given."
  ([cursor v]
    (transact! cursor [] (fn [_] v) nil))
  ([cursor korks v]
    (transact! cursor korks (fn [_] v) nil))
  ([cursor korks v tag]
    (transact! cursor korks (fn [_] v) tag)))

(defn get-node
  "A helper function to get at React refs. Given a owning pure node
  extract the ref specified by name."
  ([owner]
     (.getDOMNode owner))
  ([owner name]
     (when-let [refs (.-refs owner)]
       (.getDOMNode (aget refs name)))))

(defn set-state!
  "Takes a pure owning component, a sequential list of keys and value and
   sets the state of the component. Conceptually analagous to React
   setState. Will schedule an Om re-render."
  ([owner v]
     (-set-state! owner v true))
  ([owner korks v]
     (let [ks (if (sequential? korks) korks [korks])]
       (-set-state! owner ks v true))))

(defn set-state-nr!
  "EXPERIMENTAL: Same as set-state! but does not trigger re-render."
  ([owner v]
     (-set-state! owner v false))
  ([owner korks v]
     (let [ks (if (sequential? korks) korks [korks])]
       (-set-state! owner ks v false))))

(defn update-state!
  "Takes a pure owning component, a sequential list of keys and a
   function to transition the state of the component. Conceptually
   analagous to React setState. Will schedule an Om re-render."
  ([owner f]
     (set-state! owner (f (get-state owner))))
  ([owner korks f]
     (set-state! owner korks (f (get-state owner korks)))))

(defn update-state-nr!
  "EXPERIMENTAL: Same as update-state! but does not trigger re-render."
  ([owner f]
     (set-state-nr! owner (f (get-state owner))))
  ([owner korks f]
     (set-state-nr! owner korks (f (get-state owner korks)))))

(defn refresh!
  "Utility to re-render an owner."
  [owner]
  (update-state! owner identity))

(defn get-render-state
  "Takes a pure owning component and an optional key or sequential
   list of keys and returns a property in the component local state if
   it exists. Always returns the rendered state, not the pending
   state."
  ([owner]
     (-get-render-state owner))
  ([owner korks]
     (let [ks (if (sequential? korks) korks [korks])]
       (-get-render-state owner ks))))

(defn rendering?
  "Returns true if in the React render phase."
  []
  (true? *read-enabled*))

