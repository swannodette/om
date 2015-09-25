(ns om.core
  (:require-macros om.core)
  (:require [cljsjs.react]
            [om.dom :as dom :include-macros true]
            [goog.object :as gobj]
            [goog.dom :as gdom]
            [goog.dom.dataset :as gdomdata])
  (:import [goog.ui IdGenerator]))

(def ^{:dynamic true :private true} *parent* nil)
(def ^{:dynamic true :private true} *instrument* nil)
(def ^{:dynamic true :private true} *descriptor* nil)
(def ^{:dynamic true :private true} *state* nil)
(def ^{:dynamic true :private true} *root-key* nil)

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

(defprotocol IRenderProps
  (render-props [this props state]))

(defprotocol IRenderState
  (render-state [this state]))

;; marker protocol, if set component will check equality of current
;; and render state
(defprotocol ICheckState) 

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

;; PRIVATE render queue, for components that use local state
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

;; PRIVATE
(defprotocol INotify
  (-listen! [x key tx-listen])
  (-unlisten! [x key])
  (-notify! [x tx-data root-cursor]))

;; PRIVATE
(defprotocol IRootProperties
  (-set-property! [this id p val])
  (-remove-property! [this id p])
  (-remove-properties! [this id])
  (-get-property [this id p]))

;; PRIVATE
(defprotocol IRootKey
  (-root-key [cursor]))

(defprotocol IAdapt
  (-adapt [this other]))

(extend-type default
  IAdapt
  (-adapt [_ other]
    other))

(defn adapt [x other]
  (-adapt x other))

(defprotocol IOmRef
  (-add-dep! [this c])
  (-remove-dep! [this c])
  (-refresh-deps! [this])
  (-get-deps [this]))

(declare notify*)

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

(defn component? [x]
  (aget x "isOmComponent"))

(defn ^:private children [node]
  (let [c (.. node -props -children)]
    (if (ifn? c)
      (set! (.. node -props -children) (c node))
      c)))

(defn get-props
  "Given an owning Pure node return the Om props. Analogous to React
   component props."
  ([x]
   {:pre [(component? x)]}
   (aget (.-props x) "__om_cursor"))
  ([x korks]
   {:pre [(component? x)]}
   (let [korks (if (sequential? korks) korks [korks])]
     (cond-> (aget (.-props x) "__om_cursor")
       (seq korks) (get-in korks)))))

(defn get-state
  "Returns the component local state of an owning component. owner is
   the component. An optional key or sequence of keys may be given to
   extract a specific value. Always returns pending state."
  ([owner]
   {:pre [(component? owner)]}
   (-get-state owner))
  ([owner korks]
   {:pre [(component? owner)]}
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

(defn ref-changed? [ref]
  (let [val  (value ref)
        val' (get-in @(state ref) (path ref) ::not-found)]
    (not= val val')))

(defn update-refs [c]
  (let [cstate (.-state c)
        refs   (aget cstate "__om_refs")]
    (when-not (zero? (count refs))
      (aset cstate "__om_refs"
        (into #{}
          (remove nil?
            (map
              (fn [ref]
                (let [ref-val   (value ref)
                      ref-state (state ref)
                      ref-path  (path ref)
                      ref-val'  (get-in @ref-state ref-path ::not-found)]
                  (when (not= ref-val ::not-found)
                    (if (not= ref-val ref-val')
                      (adapt ref (to-cursor ref-val' ref-state ref-path))
                      ref))))
              refs)))))))

(declare unobserve)

(def pure-methods
  {:isOmComponent true
   :getDisplayName
   (fn []
     (this-as this
       (let [c (children this)]
         (when (satisfies? IDisplayName c)
           (display-name c)))))
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
                             (init-state c))
                           (dissoc istate ::id))}]
         (aset props "__om_init_state" nil)
         ret)))
   :shouldComponentUpdate
   (fn [next-props next-state]
     (this-as this
       (let [props (.-props this)
             state (.-state this)
             c     (children this)]
         ;; need to merge in props state first
         (merge-props-state this next-props)
         (if (satisfies? IShouldUpdate c)
           (should-update c
             (get-props #js {:props next-props :isOmComponent true})
             (-get-state this))
           (let [cursor      (aget props "__om_cursor")
                 next-cursor (aget next-props "__om_cursor")]
             (cond
               (not= (-value cursor) (-value next-cursor))
               true

               (and (cursor? cursor) (cursor? next-cursor)
                    (not= (-path cursor) (-path next-cursor )))
               true
               
               (not= (-get-state this) (-get-render-state this))
               true

               (and (not (zero? (count (aget state "__om_refs"))))
                    (some #(ref-changed? %) (aget state "__om_refs")))
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
           (will-mount c)))
       (merge-pending-state this)))
   :componentDidMount
   (fn []
     (this-as this
       (let [c (children this)
             cursor (aget (.-props this) "__om_cursor")]
         (when (satisfies? IDidMount c)
           (did-mount c)))))
   :componentWillUnmount
   (fn []
     (this-as this
       (let [c (children this)
             cursor (aget (.-props this) "__om_cursor")]
         (when (satisfies? IWillUnmount c)
           (will-unmount c))
         (when-let [refs (seq (aget (.-state this) "__om_refs"))]
           (doseq [ref refs]
             (unobserve this ref))))))
   :componentWillUpdate
   (fn [next-props next-state]
     (this-as this
       (let [c (children this)]
         (when (satisfies? IWillUpdate c)
           (let [state (.-state this)]
             (will-update c
               (get-props #js {:props next-props :isOmComponent true})
               (-get-state this)))))
       (merge-pending-state this)
       (update-refs this)))
   :componentDidUpdate
   (fn [prev-props prev-state]
     (this-as this
       (let [c (children this)]
         (when (satisfies? IDidUpdate c)
           (let [state (.-state this)]
             (did-update c
               (get-props #js {:props prev-props :isOmComponent true})
               (or (aget state "__om_prev_state")
                 (aget state "__om_state")))))
         (aset (.-state this) "__om_prev_state" nil))))
   :componentWillReceiveProps
   (fn [next-props]
     (this-as this
       (let [c (children this)]
         (when (satisfies? IWillReceiveProps c)
           (will-receive-props c
             (get-props #js {:props next-props :isOmComponent true}))))))
   :render
   (fn []
     (this-as this
       (let [c (children this)
             props (.-props this)]
         (binding [*parent*     this
                   *state*      (aget props "__om_app_state")
                   *instrument* (aget props "__om_instrument")
                   *descriptor* (aget props "__om_descriptor")
                   *root-key*   (aget props "__om_root_key")]
           (cond
             (satisfies? IRender c)
             (render c)

             (satisfies? IRenderProps c)
             (render-props c (aget props "__om_cursor") (get-state this))

             (satisfies? IRenderState c)
             (render-state c (get-state this))
             :else c)))))})

(defn specify-state-methods! [obj]
  (specify! obj
    ISetState
    (-set-state!
      ([this val render]
         (let [props     (.-props this)
               app-state (aget props "__om_app_state")]
           (aset (.-state this) "__om_pending_state" val)
           (when (and (not (nil? app-state)) render)
             (-queue-render! app-state this))))
      ([this ks val render]
         (let [props     (.-props this)
               state     (.-state this)
               pstate    (-get-state this)
               app-state (aget props "__om_app_state")]
           (aset state "__om_pending_state" (assoc-in pstate ks val))
           (when (and (not (nil? app-state)) render)
             (-queue-render! app-state this)))))
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
;; EXPERIMENTAL: No Local State

(declare get-node)

(defn react-id [x]
  (let [id (gdomdata/get (get-node x) "reactid")]
    (assert id)
    id))

(defn get-gstate [owner]
  (aget (.-props owner) "__om_app_state"))

(defn no-local-merge-pending-state [owner]
  (let [gstate (get-gstate owner)
        spath  [:state-map (react-id owner)]
        states (get-in @gstate spath)]
    (when (:pending-state states)
      (swap! gstate update-in spath
        (fn [states]
          (-> states
            (assoc :previous-state (:render-state states))
            (assoc :render-state
              (merge (:render-state states) (:pending-state states)))
            (dissoc :pending-state)))))))

(declare mounted?)

(def no-local-state-methods
  (assoc pure-methods
    :getInitialState
    (fn []
      (this-as this
        (let [c      (children this)
              props  (.-props this)
              istate (or (aget props "__om_init_state") {})
              om-id  (or (:om.core/id istate)
                         (.getNextUniqueId (.getInstance IdGenerator)))
              state  (merge (dissoc istate ::id)
                       (when (satisfies? IInitState c)
                         (init-state c)))]
          (aset props "__om_init_state" nil)
          #js {:__om_id om-id})))
    :componentDidMount
    (fn []
      (this-as this
        (let [c      (children this)
              cursor (aget (.-props this) "__om_cursor")
              spath  [:state-map (react-id this) :render-state]]
          (swap! (get-gstate this) assoc-in spath state)
          (when (satisfies? IDidMount c)
            (did-mount c)))))
    :componentWillMount
    (fn []
      (this-as this
        (merge-props-state this)
        (let [c (children this)]
          (when (satisfies? IWillMount c)
            (will-mount c)))
        ;; TODO: cannot merge state until mounted?
        (when (mounted? this)
          (no-local-merge-pending-state this))))
    :componentWillUnmount
    (fn []
      (this-as this
        (let [c (children this)]
          (when (satisfies? IWillUnmount c)
            (will-unmount c))
          (swap! (get-gstate this) update-in [:state-map] dissoc (react-id this))
          (when-let [refs (seq (aget (.-state this) "__om_refs"))]
            (doseq [ref refs]
              (unobserve this ref))))))
   :componentWillUpdate
   (fn [next-props next-state]
     (this-as this
       (let [props  (.-props this)
             c      (children this)]
         (when (satisfies? IWillUpdate c)
           (let [state (.-state this)]
             (will-update c
               (get-props #js {:props next-props :isOmComponent true})
               (-get-state this)))))
       (no-local-merge-pending-state this)
       (update-refs this)))
    :componentDidUpdate
    (fn [prev-props prev-state]
      (this-as this
        (let [c      (children this)
              gstate (get-gstate this)
              states (get-in @gstate [:state-map (react-id this)])
              spath  [:state-map (react-id this)]]
          (when (satisfies? IDidUpdate c)
            (let [state (.-state this)]
              (did-update c
                (get-props #js {:props prev-props :isOmComponent true})
                (or (:previous-state states)
                    (:render-state states)))))
          (when (:previous-state states)
            (swap! gstate update-in spath dissoc :previous-state)))))))

(defn no-local-descriptor [methods]
  (specify! (clj->js methods)
    ISetState
    (-set-state!
      ([this val render]
         (let [gstate (get-gstate this)
               spath  [:state-map (react-id this) :pending-state]]
           (swap! (get-gstate this) assoc-in spath val)
           (when (and (not (nil? gstate)) render)
             (-queue-render! gstate this))))
      ([this ks val render]
         (-set-state! this (assoc-in (-get-state this) ks val) render)))
    IGetRenderState
    (-get-render-state
      ([this]
         (let [spath [:state-map (react-id this) :render-state]]
           (get-in @(get-gstate this) spath)))
      ([this ks]
         (get-in (-get-render-state this) ks)))
    IGetState
    (-get-state
      ([this]
        (if (mounted? this)
          (let [spath [:state-map (react-id this)]
               states (get-in @(get-gstate this) spath)]
            (or (:pending-state states)
                (:render-state states)))
          ;; TODO: means state cannot be written to until mounted - David
          (aget (.-props this) "__om_init_state")))
      ([this ks]
         (get-in (-get-state this) ks)))))

;; =============================================================================
;; Cursors

(defn valid? [x]
  (if (satisfies? ICursor x)
    (not (keyword-identical? @x ::invalid))
    true))

(deftype MapCursor [value state path]
  IWithMeta
  (-with-meta [_ new-meta]
    (MapCursor. (with-meta value new-meta) state path))
  IMeta
  (-meta [_] (meta value))
  IDeref
  (-deref [this]
    (get-in @state path ::invalid))
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
    (-count value))
  ICollection
  (-conj [_ o]
    (MapCursor. (-conj value o) state path))
  ;; EXPERIMENTAL
  IEmptyableCollection
  (-empty [_]
    (MapCursor. (empty value) state path))
  ILookup
  (-lookup [this k]
    (-lookup this k nil))
  (-lookup [this k not-found]
    (let [v (-lookup value k ::not-found)]
      (if-not (= v ::not-found)
        (-derive this v state (conj path k))
        not-found)))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  ISeqable
  (-seq [this]
    (when (pos? (count value))
      (map (fn [[k v]] [k (-derive this v state (conj path k))]) value)))
  IAssociative
  (-contains-key? [_ k]
    (-contains-key? value k))
  (-assoc [_ k v]
    (MapCursor. (-assoc value k v) state path))
  IMap
  (-dissoc [_ k]
    (MapCursor. (-dissoc value k) state path))
  IEquiv
  (-equiv [_ other]
    (if (cursor? other)
      (= value (-value other))
      (= value other)))
  IHash
  (-hash [_]
    (hash value))
  IKVReduce
  (-kv-reduce [_ f init]
    (-kv-reduce value f init))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-pr-writer value writer opts)))

(deftype IndexedCursor [value state path]
  ISequential
  IDeref
  (-deref [this]
    (get-in @state path ::invalid))
  IWithMeta
  (-with-meta [_ new-meta]
    (IndexedCursor. (with-meta value new-meta) state path))
  IMeta
  (-meta [_] (meta value))
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
    (-count value))
  ICollection
  (-conj [_ o]
    (IndexedCursor. (-conj value o) state path))
  ;; EXPERIMENTAL
  IEmptyableCollection
  (-empty [_]
    (IndexedCursor. (empty value) state path))
  ILookup
  (-lookup [this n]
    (-nth this n nil))
  (-lookup [this n not-found]
    (-nth this n not-found))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  IIndexed
  (-nth [this n]
    (-derive this (-nth value n) state (conj path n)))
  (-nth [this n not-found]
    (if (< n (-count value))
      (-derive this (-nth value n not-found) state (conj path n))
      not-found))
  ISeqable
  (-seq [this]
    (when (pos? (count value))
      (map (fn [v i] (-derive this v state (conj path i))) value (range))))
  IAssociative
  (-contains-key? [_ k]
    (-contains-key? value k))
  (-assoc [this n v]
    (-derive this (-assoc-n value n v) state path))
  IStack
  (-peek [this]
    (-derive this (-peek value) state path))
  (-pop [this]
    (-derive this (-pop value) state path))
  IEquiv
  (-equiv [_ other]
    (if (cursor? other)
      (= value (-value other))
      (= value other)))
  IHash
  (-hash [_]
    (hash value))
  IKVReduce
  (-kv-reduce [_ f init]
    (-kv-reduce value f init))
  IPrintWithWriter
  (-pr-writer [_ writer opts]
    (-pr-writer value writer opts)))

(defn ^:private to-cursor* [val state path]
  (specify val
    IDeref
    (-deref [this]
      (get-in @state path ::invalid))
    ICursor
    (-path [_] path)
    (-state [_] state)
    ITransact
    (-transact! [this korks f tag]
      (transact* state this korks f tag))
    IEquiv
    (-equiv [_ other]
      (if (cursor? other)
        (= val (-value other))
        (= val other)))))

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
;; Ref Cursors

(declare commit! id refresh-props!)

(defn root-cursor
  "Given an application state atom return a root cursor for it."
  [atom]
  {:pre [(satisfies? IDeref atom)]}
  (to-cursor @atom atom []))

(def _refs (atom {}))

(defn ref-sub-cursor [x parent]
  (specify x
    ICloneable
    (-clone [this]
      (ref-sub-cursor (clone x) parent))
    IAdapt
    (-adapt [this other]
      (ref-sub-cursor (adapt x other) parent))
    ICursorDerive
    (-derive [this derived state path]
      (let [cursor' (to-cursor derived state path)]
        (if (cursor? cursor')
          (adapt this cursor')
          cursor')))
    ITransact
    (-transact! [cursor korks f tag]
      (commit! cursor korks f)
      (-refresh-deps! parent))))

(defn ref-cursor? [x]
  (satisfies? IOmRef x))

(defn ref-cursor
  "Given a cursor return a reference cursor that inherits all of the
  properties and methods of the cursor. Reference cursors may be
  observed via om.core/observe."
  [cursor]
  {:pre [(cursor? cursor)]}
  (if (ref-cursor? cursor)
    cursor
    (let [path    (path cursor)
          storage (get
                    (swap! _refs update-in
                      [path] (fnil identity (atom {})))
                    path)]
      (specify cursor
        IAdapt
        (-adapt [_ other]
          (ref-cursor other))
        ICursorDerive
        (-derive [this derived state path]
          (let [cursor' (to-cursor derived state path)]
            (if (cursor? cursor')
              (ref-sub-cursor cursor' this)
              cursor')))
        IOmRef
        (-add-dep! [_ c]
          (swap! storage assoc (id c) c))
        (-remove-dep! [_ c]
          (let [m (swap! storage dissoc (id c))]
            (when (zero? (count m))
              (swap! _refs dissoc path))))
        (-refresh-deps! [_]
          (doseq [c (vals @storage)]
            (-queue-render! (state cursor) c)))
        (-get-deps [_]
          @storage)
        ITransact
        (-transact! [cursor korks f tag]
          (commit! cursor korks f)
          (-refresh-deps! cursor))))))

(defn same-ref? [a b]
  (and
   (identical? (-state a) (-state b))
   (= (-path a) (-path b))))

(defn have-ref? [refs ref]
  (some #(same-ref? ref %) refs))

(defn add-ref-to-component! [c ref]
  (let [state (.-state c)
        refs  (or (aget state "__om_refs") #{})]
    (when-not (have-ref? refs ref)
      (aset state "__om_refs" (conj refs ref)))))

(defn remove-ref-from-component! [c ref]
  (let [state (.-state c)
        refs  (aget state "__om_refs")]
    (when (have-ref? refs ref)
      (aset state "__om_refs" (remove #(same-ref? ref %) refs)))))

(defn observe
  "Given a component and a reference cursor have the component observe
  the reference cursor for any data changes."
  [c ref]
  {:pre [(component? c) (cursor? ref) (ref-cursor? ref)]}
  (add-ref-to-component! c ref)
  (-add-dep! ref c)
  ref)

(defn unobserve [c ref]
  (remove-ref-from-component! c ref)
  (-remove-dep! ref c)
  ref)

;; =============================================================================
;; API

(def ^:private refresh-queued false)
(def ^:private refresh-set (atom #{}))

(defn ^:private get-renderT [state]
  (or (.-om$render$T state) 0))

(defn render-all
  "Force a render of *all* roots. Usage of this function is almost
  never recommended."
  ([] (render-all nil))
  ([state]
   (set! refresh-queued false)
   (doseq [f @refresh-set] (f))
   (when-not (nil? state)
     (set! (.-om$render$T state) (inc (get-renderT state))))))

(def ^:private roots (atom {}))

(defn ^:private valid-component? [x f]
  (assert
    (or (satisfies? IRender x)
        (satisfies? IRenderProps x)
        (satisfies? IRenderState x))
    (str "Invalid Om component fn, " (.-name f)
         " does not return valid instance")))

(defn ^:private valid-opts? [m]
  (every? #{:key :react-key :key-fn :fn :init-state :state
            :opts :shared ::index :instrument :descriptor}
    (keys m)))

(defn id [owner]
  (aget (.-state owner) "__om_id"))

(defn get-descriptor
  ([f] (get-descriptor f nil))
  ([f descriptor]
   (let [rdesc (or descriptor *descriptor* pure-descriptor)]
     (when (or (nil? (gobj/get f "om$descriptor"))
               (not (identical? rdesc (gobj/get f "om$tag"))))
       (let [factory (js/React.createFactory (js/React.createClass rdesc))]
         (gobj/set f "om$descriptor" factory)
         (gobj/set f "om$tag" rdesc))))
   (gobj/get f "om$descriptor")))

(defn getf
  ([f cursor]
   (if (instance? MultiFn f)
     (let [dv ((.-dispatch-fn f) cursor nil)]
      (get-method f dv))
     f))
  ([f cursor opts]
   (if (instance? MultiFn f)
     (let [dv ((.-dispatch-fn f) cursor nil opts)]
       (get-method f dv))
     f)))

(defn build*
  ([f cursor] (build* f cursor nil))
  ([f cursor m]
   {:pre [(ifn? f) (or (nil? m) (map? m))]}
   (assert (valid-opts? m)
     (apply str "build options contains invalid keys, only :key, :key-fn :react-key, "
       ":fn, :init-state, :state, and :opts allowed, given "
       (interpose ", " (keys m))))
   (cond
     (nil? m)
     (let [shared (get-shared *parent*)
           ctor   (get-descriptor (getf f cursor))]
       (ctor #js {:__om_cursor cursor
                  :__om_shared shared
                  :__om_root_key *root-key*
                  :__om_app_state *state*
                  :__om_descriptor *descriptor*
                  :__om_instrument *instrument*
                  :children
                  (fn [this]
                    (let [ret (f cursor this)]
                      (valid-component? ret f)
                      ret))}))

     :else
     (let [{:keys [key key-fn state init-state opts]} m
           dataf   (get m :fn)
           cursor' (if-not (nil? dataf)
                     (if-let [i (::index m)]
                       (dataf cursor i)
                       (dataf cursor))
                     cursor)
           rkey    (cond
                     (not (nil? key)) (get cursor' key)
                     (not (nil? key-fn)) (key-fn cursor')
                     :else (get m :react-key))
           shared  (or (:shared m) (get-shared *parent*))
           ctor    (get-descriptor (getf f cursor' opts) (:descriptor m))]
       (ctor #js {:__om_cursor cursor'
                  :__om_index (::index m)
                  :__om_init_state init-state
                  :__om_state state
                  :__om_shared shared
                  :__om_root_key *root-key*
                  :__om_app_state *state*
                  :__om_descriptor *descriptor*
                  :__om_instrument *instrument*
                  :key (or rkey js/undefined) ;; annoying
                  :children
                  (if (nil? opts)
                    (fn [this]
                      (let [ret (f cursor' this)]
                        (valid-component? ret f)
                        ret))
                    (fn [this]
                      (let [ret (f cursor' this opts)]
                        (valid-component? ret f)
                        ret)))})))))

(defn build
  "Builds an Om component. Takes an IRender/IRenderState instance
   returning function f, a value, and an optional third argument
   which may be a map of build specifications.

   f - is a function of 2 or 3 arguments. The first argument can be
   any value and the second argument will be the owning pure node.
   If a map of options m is passed in this will be the third
   argument. f must return at a minimum an IRender or IRenderState
   instance, this instance may implement other React life cycle
   protocols.

   x - any value

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

     (build list-of-gadgets x
        {:init-state {:event-chan ...
                      :narble ...}})
  "
  ([f x] (build f x nil))
  ([f x m]
   {:pre [(ifn? f) (or (nil? m) (map? m))]}
   (if-not (nil? *instrument*)
     (let [ret (*instrument* f x m)]
       (if (= ret ::pass)
         (build* f x m)
         ret))
     (build* f x m))))

(defn build-all
  "Build a sequence of components. f is the component constructor
   function, xs a sequence of values, and m a map of options the
   same as provided to om.core/build."
  ([f xs] (build-all f xs nil))
  ([f xs m]
   {:pre [(ifn? f) (or (nil? m) (map? m))]}
   (map (fn [x i]
          (build f x (assoc m ::index i)))
     xs (range))))

(defn ^:private setup [state key tx-listen]
  (when-not (satisfies? INotify state)
    (let [properties   (atom {})
          listeners    (atom {})
          render-queue (atom #{})]
      (specify! state
        IRootProperties
        (-set-property! [_ id k v]
          (swap! properties assoc-in [id k] v))
        (-remove-property! [_ id k]
          (swap! properties dissoc id k))
        (-remove-properties! [_ id]
          (swap! properties dissoc id))
        (-get-property [_ id k]
          (get-in @properties [id k]))
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

(defn tag-root-key [cursor root-key]
  (if (cursor? cursor)
    (specify cursor
      ICloneable
      (-clone [this]
        (tag-root-key (clone cursor) root-key))
      IAdapt
      (-adapt [this other]
        (tag-root-key (adapt cursor other) root-key))
      IRootKey
      (-root-key [this] root-key))
    cursor))

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
   :adapt      - a function to adapt the root cursor
   :raf        - override requestAnimationFrame based rendering. If
                 false setTimeout will be use. If given a function
                 will be invoked instead.

   Example:

   (root
     (fn [data owner]
       ...)
     {:message :hello}
     {:target js/document.body})"
  ([f value {:keys [target tx-listen path instrument descriptor adapt raf] :as options}]
    (assert (ifn? f) "First argument must be a function")
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
          adapt (or adapt identity)
          m     (dissoc options :target :tx-listen :path :adapt :raf)
          ret   (atom nil)
          rootf (fn rootf []
                  (swap! refresh-set disj rootf)
                  (let [value  @state
                        cursor (adapt
                                 (tag-root-key
                                   (if (nil? path)
                                     (to-cursor value state [])
                                     (to-cursor (get-in value path) state path))
                                   watch-key))]
                    (when-not (-get-property state watch-key :skip-render-root)
                      (-set-property! state watch-key :skip-render-root true)
                      (let [c (dom/render
                                (binding [*descriptor* descriptor
                                          *instrument* instrument
                                          *state*      state
                                          *root-key*   watch-key]
                                  (build f cursor m))
                                  target)]
                        (when (nil? @ret)
                          (reset! ret c))))
                    ;; update state pass
                    (let [queue (-get-queue state)]
                      (-empty-queue! state)
                      (when-not (empty? queue)
                        (doseq [c queue]
                          (when (.isMounted c)
                            (when-let [next-props (aget (.-state c) "__om_next_cursor")]
                              (aset (.-props c) "__om_cursor" next-props)
                              (aset (.-state c) "__om_next_cursor" nil))
                            (when (or (not (satisfies? ICheckState (children c)))
                                      (.shouldComponentUpdate c (.-props c) (.-state c)))
                              (.forceUpdate c))))))
                    ;; ref cursor pass
                    (let [_refs @_refs]
                      (when-not (empty? _refs)
                        (doseq [[path cs] _refs]
                          (let [cs @cs]
                            (doseq [[id c] cs]
                              (when (.shouldComponentUpdate c (.-props c) (.-state c))
                                (.forceUpdate c)))))))
                    @ret))]
      (add-watch state watch-key
        (fn [_ _ o n]
          (when (and (not (-get-property state watch-key :ignore))
                     (not (identical? o n)))
            (-set-property! state watch-key :skip-render-root false))
          (-set-property! state watch-key :ignore false)
          (when-not (contains? @refresh-set rootf)
            (swap! refresh-set conj rootf))
          (when-not refresh-queued
            (set! refresh-queued true)
            (cond
              (fn? raf)
              (raf)

              (or (false? raf)
                  (not (exists? js/requestAnimationFrame)))
              (js/setTimeout #(render-all state) 16)

              :else
              (js/requestAnimationFrame #(render-all state))))))
      ;; store fn to remove previous root render loop
      (swap! roots assoc target
        (fn []
          (-remove-properties! state watch-key)
          (remove-watch state watch-key)
          (tear-down state watch-key)
          (swap! refresh-set disj rootf)
          (swap! roots dissoc target)
          (js/React.unmountComponentAtNode target)))
      (rootf))))

(defn detach-root
  "Given a DOM target remove its render loop if one exists."
  [target]
  {:pre [(gdom/isElement target)]}
  (when-let [f (get @roots target)]
    (f)))

(defn transactable? [x]
  (satisfies? ITransact x))

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
   {:pre [(transactable? cursor) (ifn? f)]}
   (let [korks (cond
                 (nil? korks) []
                 (sequential? korks) korks
                 :else [korks])]
     (-transact! cursor korks f tag))))

(defn update!
  "Like transact! but no function provided, instead a replacement
  value is given."
  ([cursor v]
   {:pre [(cursor? cursor)]}
   (transact! cursor [] (fn [_] v) nil))
  ([cursor korks v]
   {:pre [(cursor? cursor)]}
   (transact! cursor korks (fn [_] v) nil))
  ([cursor korks v tag]
   {:pre [(cursor? cursor)]}
   (transact! cursor korks (fn [_] v) tag)))

(defn commit!
  "EXPERIMENTAL: Like transact! but does not schedule a re-render or
  create a transact event."
  [cursor korks f]
  {:pre [(cursor? cursor) (ifn? f)]}
  (let [key       (when (satisfies? IRootKey cursor)
                    (-root-key cursor))
        app-state (state cursor)
        korks     (cond
                    (nil? korks) []
                    (sequential? korks) korks
                    :else [korks])
        cpath     (path cursor)
        rpath     (into cpath korks)]
    (when key
      (-set-property! app-state key :ignore true))
    (if (empty? rpath)
      (swap! app-state f)
      (swap! app-state update-in rpath f))))

(defn get-node
  "A helper function to get at React DOM refs. Given a owning pure node
  extract the DOM ref specified by name."
  ([owner]
   (.getDOMNode owner))
  ([owner name]
   {:pre [(string? name)]}
   (some-> (.-refs owner) (aget name) (.getDOMNode))))

(defn get-ref
  "A helper function to get at React refs. Given an owning pure node extract
  the ref specified by name."
  [owner name]
  {:pre [(component? owner) (string? name)]}
  (some-> (.-refs owner) (gobj/get name)))

(defn mounted?
  "Return true if the backing React component is mounted into the DOM."
  [owner]
  (.isMounted owner))

(defn set-state!
  "Takes a pure owning component, a sequential list of keys and value and
   sets the state of the component. Conceptually analagous to React
   setState. Will schedule an Om re-render."
  ([owner v]
   {:pre [(component? owner)]}
   (-set-state! owner v true))
  ([owner korks v]
   {:pre [(component? owner)]}
   (let [ks (if (sequential? korks) korks [korks])]
     (-set-state! owner ks v true))))

(defn set-state-nr!
  "EXPERIMENTAL: Same as set-state! but does not trigger re-render."
  ([owner v]
   {:pre [(component? owner)]}
   (-set-state! owner v false))
  ([owner korks v]
   {:pre [(component? owner)]}
   (let [ks (if (sequential? korks) korks [korks])]
     (-set-state! owner ks v false))))

(defn update-state!
  "Takes a pure owning component, a sequential list of keys and a
   function to transition the state of the component. Conceptually
   analagous to React setState. Will schedule an Om re-render."
  ([owner f]
   {:pre [(component? owner) (ifn? f)]}
   (set-state! owner (f (get-state owner))))
  ([owner korks f]
   {:pre [(component? owner) (ifn? f)]}
   (set-state! owner korks (f (get-state owner korks)))))

(defn update-state-nr!
  "EXPERIMENTAL: Same as update-state! but does not trigger re-render."
  ([owner f]
   {:pre [(component? owner) (ifn? f)]}
   (set-state-nr! owner (f (get-state owner))))
  ([owner korks f]
   {:pre [(component? owner) (ifn? f)]}
   (set-state-nr! owner korks (f (get-state owner korks)))))

(defn refresh!
  "Utility to re-render an owner."
  [owner]
  {:pre [(component? owner)]}
  (update-state! owner identity))

(defn get-render-state
  "Takes a pure owning component and an optional key or sequential
   list of keys and returns a property in the component local state if
   it exists. Always returns the rendered state, not the pending
   state."
  ([owner]
   {:pre [(component? owner)]}
   (-get-render-state owner))
  ([owner korks]
   {:pre [(component? owner)]}
   (let [ks (if (sequential? korks) korks [korks])]
     (-get-render-state owner ks))))

