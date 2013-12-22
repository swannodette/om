(ns om.core
  (:require-macros [om.core :refer [pure component]])
  (:require [om.dom :as dom :include-macros true]))

;; =============================================================================
;; React Life Cycle Protocols
;; See http://facebook.github.io/react/docs/component-specs.html#lifecycle-methods

(defprotocol IInitState
  (init-state [this owner]))

(defprotocol IShouldUpdate
  (should-update [this owner next-props next-state]))

(defprotocol IWillMount
  (will-mount [this owner]))

(defprotocol IDidMount
  (did-mount [this owner node]))

(defprotocol IWillUnmount
  (will-unmount [this owner]))

(defprotocol IWillUpdate
  (will-update [this owner next-props next-state]))

(defprotocol IDidUpdate
  (did-update [this owner prev-props prev-state root-node]))

(defprotocol IRender
  (render [this owner]))

;; =============================================================================
;; A Truly Pure Component
;; 
;; This React class takes an immutable value as its props and an instance that
;; must at a minimum implement IRender as its children.
;;
;; (Pure. {:foo "bar"} irender-instance)

(def ^:private Pure
  (js/React.createClass
    #js {:getInitialState
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               #js {:__om_state
                    (merge {}
                      (when (satisfies? IInitState c)
                        (init-state c this)))})))
         :shouldComponentUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (if (satisfies? IShouldUpdate c)
                 (should-update c this next-props next-state)
                 (not (identical? (aget (.-props this) "value")
                                  (aget next-props "value")))))))
         :componentWillMount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillMount c)
                 (will-mount c this)))))
         :componentDidMount
         (fn [node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidMount c)
                 (did-mount c this node)))))
         :componentWillUnmount
         (fn []
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUnmount c)
                 (will-unmount c this)))))
         :componentWillUpdate
         (fn [next-props next-state]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IWillUpdate c)
                 (will-update c this next-props next-state)))))
         :componentDidUpdate
         (fn [prev-props prev-state root-node]
           (this-as this
             (let [c (.. this -props -children)]
               (when (satisfies? IDidUpdate c)
                 (did-update c this prev-props prev-state root-node)))))
         :render
         (fn []
           (this-as this
             (render (.. this -props -children) this)))}))

(def ^:private refresh-queued false)

(defn root
  "Takes an immutable value or value wrapped in an atom, an initial
   function f, and a DOM target. Installs an Om/React render loop. f
   must return an instance that at a minimum implements IRender (it
   may implement other React life cycle protocols). f must take a
   single argument which will be the root cursor. A cursor is simply
   data that has been annotated via metadata with state and path
   information - :om.core/state and :om.core/path respectively.

   Example:

   (root {:message :hello}
     (fn [data]
       ...)
     js/document.body)"
  [value f target]
  (let [state (if (instance? Atom value)
                value
                (atom value))
        rootf (fn []
                (set! refresh-queued false)
                (let [path []]
                  (dom/render
                    (pure #js {:value @state}
                      (f (with-meta @state {::state state ::path path})))
                    target)))]
    (add-watch state ::root
      (fn [_ _ _ _]
        (when-not refresh-queued
          (set! refresh-queued true)
          (if (exists? js/requestAnimationFrame)
            (js/requestAnimationFrame rootf)
            (js/setTimeout rootf 16)))))
    (rootf)))

(defn build
  "Builds a Om component. Takes an IRender instance returning function
   f, a cursor, and an optional third argument which may be a vector
   representing the path or a map of build specifications. 

   f - is a function of two arguments the first argument will be the
   cursor and the second argument will be a map of optional values
   passed to build. f must return at a minimum an IRender instance,
   this instance may implement other React life cycle protocols.

   cursor - an immutable value annotated with :om.core/state and
   :om.core/path

   sorm - If a vector, specifies the relative path into the tree that
   the component will be built from. If a map the following
   keys are allowed:

     :path      - the relative path in the tree to build the component with
     :abs-path  - an absolute path from the root
     :key       - a keyword that should be used to look up the key used by
                  React itself when rendering sequential things.
     :react-key - an explicit react key
     :fn        - a function to apply to the data at the relative path before
                  invoking f.
     :opts      - a map of options to pass to the component.

   Example:

     (build list-of-gadgets cursor
        {:path [:gadgets]
         :opts {:event-chan ...
                :narble ...}})
  "
  ([f cursor] (build f cursor nil))
  ([f cursor sorm]
    (cond
      (nil? sorm)
      (pure #js {:value cursor} (f cursor))

      (sequential? sorm)
      (let [data    (get-in cursor sorm)
            cursor' (with-meta data (update-in (meta cursor) [::path] into sorm))]
        (pure #js {:value data} (f cursor')))

      :else
      (let [{:keys [path key react-key opts]} sorm
            dataf   (get sorm :fn)
            path    (if (nil? path) (:abs-path sorm) path)
            data    (get-in cursor path)
            data    (if-not (nil? dataf) (dataf data) data)
            rkey    (if-not (nil? key)
                      (get data key)
                      (if-not (nil? react-key)
                        react-key))
            cursor' (if (contains? sorm :abs-path)
                      (with-meta data (assoc (meta cursor) ::path path))
                      (with-meta data (update-in (meta cursor) [::path] into path)))]
        (pure #js {:value data :key rkey}
          (if (nil? opts)
            (f cursor')
            (f cursor' opts)))))))

(defn update!
  "Given a cursor, a list of keys ks, mutate the tree at the path
   specified by the cursor + the keys by applying f to the specified
   value in the tree. If only given two arguments, assumed no list
   of keys was specified. An Om re-render will be triggered."
  ([cursor f]
    (let [m (meta cursor)
          path (::path m)]
      (if (empty? path)
        (swap! (::state m) f)
        (swap! (::state m) update-in path f))))
  ([cursor ks f]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f)))
  ([cursor ks f a]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a)))
  ([cursor ks f a b]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b)))
  ([cursor ks f a b c]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c)))
  ([cursor ks f a b c d]
    (let [m (meta cursor)]
      (swap! (::state m) update-in (into (::path m) ks) f a b c d)))
  ([cursor ks f a b c d & args]
    (let [m (meta cursor)]
      (apply swap! (::state m) update-in (into (::path m) ks) f a b c d args))))

(defn get-node
  "A helper function to get at React refs. Given a owning pure node
  extract the ref specified by name. Note the life cycle protocol methods
  all pass the owner as an argument, ie. IRender."
  [owner name]
  (when-let [refs (.-refs owner)]
    (.getDOMNode (aget refs name))))

(defn set-state!
  "EXPERIMENTAL"
  [owner ks v]
  (aset (.-state owner) "__om_state"
    (assoc-in (aget (.-state owner) "__om_state") ks v)))

(defn get-state
  "EXPERIMENTAL"
  [owner ks]
  (get-in (aget (.-state owner) "__om_state") ks))
