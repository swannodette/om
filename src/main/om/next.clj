(ns om.next
  (:refer-clojure :exclude [deftype])
  (:require [cljs.core :refer [deftype specify! this-as js-arguments]]
            [cljs.analyzer :as ana]))

(defn collect-statics [dt]
  (letfn [(split-on-static [forms]
            (split-with (complement '#{static}) forms))
          (split-on-symbol [forms]
            (split-with (complement symbol?) forms))]
    (loop [dt (seq dt) dt' [] statics {:fields {} :protocols []}]
      (if dt
        (let [[pre [_ sym & remaining :as post]] (split-on-static dt)
              dt' (into dt' pre)]
          (if (seq post)
            (cond
              (= sym 'field)
              (let [[field-info dt] (split-at 2 remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:fields] conj (vec field-info))))
              (symbol? sym)
              (let [[protocol-info dt] (split-on-symbol remaining)]
                (recur (seq dt) dt'
                  (update-in statics [:protocols]
                    into (concat [sym] protocol-info))))
              :else (throw (IllegalArgumentException. "Malformed static")))
            (recur nil dt' statics)))
        {:dt dt' :statics statics}))))

(def lifecycle-sigs
  '{initLocalState [this]
    componentWillReceiveProps [this next-props]
    componentWillUpdate [this next-props next-state]
    componentDidUpdate [this prev-props prev-state]
    componentWillMount [this]
    componentWillUnmount [this]
    render [this]})

(defn validate-sig [[name sig :as method]]
  (let [sig' (get lifecycle-sigs name)]
    (assert (= (count sig') (count sig))
      (str "Invalid signature for " name " got " sig ", need " sig'))))

(def reshape-map
  {:reshape
   {'initLocalState
    (fn [[name [this :as args] & body]]
      `(~name ~args
         (let [ret# (do ~@body)]
           (cljs.core/js-obj "omcljs$state" ret#))))
    'componentWillReceiveProps
    (fn [[name [this next-props :as args] & body]]
      `(~name [this# next-props#]
         (let [~this this#
               ~next-props (om.next/-next-props next-props# this#)]
           ~@body)))
    'componentWillUpdate
    (fn [[name [this next-props next-state :as args] & body]]
      `(~name [this# next-props# next-state#]
         (let [~this       this#
               ~next-props (om.next/-next-props next-props# this#)
               ~next-state (goog.object/get next-state# "omcljs$pendingState")
               ret#        (do ~@body)]
           (om.next/merge-pending-props! ~this)
           (om.next/merge-pending-state! ~this)
           ret#)))
    'componentDidUpdate
    (fn [[name [this prev-props prev-state :as args] & body]]
      `(~name [this# prev-props# prev-state#]
         (let [~this       this#
               ~prev-props (om.next/-prev-props prev-props# this#)
               ~prev-state (goog.object/get prev-state# "omcljs$previousState")]
           ~@body
           (om.next/clear-prev-props! ~this))))
    'componentWillMount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this    this#
               indexer# (get-in (om.next/get-reconciler this#) [:config :indexer])]
           (when-not (nil? indexer#)
             (om.next.protocols/index-component! indexer# ~this)))
         ~@body))
    'componentWillUnmount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this    this#
               r#       (om.next/get-reconciler this#)
               cfg#     (:config r#)
               st#      (:state cfg#)
               indexer# (:indexer cfg#)]
           (when-not (nil? st#)
             (swap! st# update-in [:om.next/queries] dissoc ~this))
           (when-not (nil? indexer#)
             (om.next.protocols/drop-component! indexer# ~this)))
         ~@body))
    'render
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this this#]
           (binding [om.next/*reconciler* (om.next/get-reconciler this#)
                     om.next/*depth*      (inc (om.next/depth this#))
                     om.next/*shared*     (om.next/shared this#)
                     om.next/*instrument* (om.next/instrument this#)
                     om.next/*parent*     this#]
            ~@body))))}
   :defaults
   `{~'isMounted
     ([this#]
       (boolean
         (goog.object/getValueByKeys this#
           "_reactInternalInstance" "_renderedComponent")))
     ~'shouldComponentUpdate
     ([this# next-props# next-state#]
       (or (not= (om.next/props this#)
                 (goog.object/get next-props# "omcljs$value"))
           (and (.. this# ~'-state)
                (not= (goog.object/get (. this# ~'-state) "omcljs$state")
                      (goog.object/get next-state# "omcljs$state")))))
     ~'componentWillUpdate
     ([this# next-props# next-state#]
       (om.next/merge-pending-props! this#)
       (om.next/merge-pending-state! this#))
     ~'componentDidUpdate
     ([this# prev-props# prev-state#]
       (om.next/clear-prev-props! this#))
     ~'componentWillMount
     ([this#]
       (let [indexer# (get-in (om.next/get-reconciler this#) [:config :indexer])]
         (when-not (nil? indexer#)
           (om.next.protocols/index-component! indexer# this#))))
     ~'componentWillUnmount
     ([this#]
       (let [r#       (om.next/get-reconciler this#)
             cfg#     (:config r#)
             st#      (:state cfg#)
             indexer# (:indexer cfg#)]
         (when-not (nil? st#)
           (swap! st# update-in [:om.next/queries] dissoc this#))
         (when-not (nil? indexer#)
           (om.next.protocols/drop-component! indexer# this#))))}})

(defn reshape [dt {:keys [reshape defaults]}]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape (first x)))
              (let [reshapef (get reshape (first x))]
                (validate-sig x)
                (reshapef x))
              x))
          (add-defaults-step [ret [name impl]]
            (if-not (some #{name} (map first (filter seq? ret)))
              (let [[before [p & after]] (split-with (complement '#{Object}) ret)]
                (into (conj (vec before) p (cons name impl)) after))
              ret))
          (add-defaults [dt]
            (reduce add-defaults-step dt defaults))
          (add-object-protocol [dt]
            (if-not (some '#{Object} dt)
              (conj dt 'Object)
              dt))]
    (->> dt (map reshape*) vec add-object-protocol add-defaults)))

(defn defui*
  ([name form] (defui* name form nil))
  ([name forms env]
   (letfn [(field-set! [obj [field value]]
             `(set! (. ~obj ~(symbol (str "-" field))) ~value))]
     (let [{:keys [dt statics]} (collect-statics forms)
           rname (if env
                   (:name (ana/resolve-var (dissoc env :locals) name))
                   name)
           ctor  `(defn ~(with-meta name {:jsdoc ["@constructor"]}) []
                    (this-as this#
                      (.apply js/React.Component this# (js-arguments))
                      (if-not (nil? (.-initLocalState this#))
                        (set! (.-state this#) (.initLocalState this#))
                        (set! (.-state this#) (cljs.core/js-obj)))
                      this#))
           ctor  (if (-> name meta :once)
                   `(when-not (cljs.core/exists? ~name)
                      ~ctor)
                   ctor)]
       `(do
          ~ctor
          (set! (.-prototype ~name) (goog.object/clone js/React.Component.prototype))
          (specify! (.-prototype ~name) ~@(reshape dt reshape-map))
          (set! (.. ~name -prototype -constructor) ~name)
          (set! (.. ~name -prototype -om$isComponent) true)
          ~@(map #(field-set! name %) (:fields statics))
          (specify! ~name ~@(:protocols statics))
          (specify! (. ~name ~'-prototype) ~@(:protocols statics))
          (set! (.-cljs$lang$type ~rname) true)
          (set! (.-cljs$lang$ctorStr ~rname) ~(str rname))
          (set! (.-cljs$lang$ctorPrWriter ~rname)
            (fn [this# writer# opt#]
              (cljs.core/-write writer# ~(str rname)))))))))

(defmacro defui [name & forms]
  (defui* name forms &env))

(defmacro ui
  [& forms]
  (let [t (with-meta (gensym "ui_") {:anonymous true})]
    `(do (defui ~t ~@forms) ~t)))

(comment
  (collect-statics
    '(static IFoo
       (foo [_])
       (bar [_])
       static field sel '[:woz ?noz]
       Object
       (toString [_])))

  (require '[clojure.pprint :refer [pprint]])

  (pprint
    (defui* 'Artist
     '(static IFoo
        (foo [_])
        (bar [_])
        static field sel '[:woz ?noz]
        Object
        (toString [_]))))

  (pprint
    (defui* 'Artist
      '(static IFoo
         (foo [_])
         (bar [_])
         static field sel '[:woz ?noz]
         Object
         (render [_ {:keys [self artists]}]
           (om.dom/div nil "Hello!"))
         (toString [_]))))

  (pprint
    (defui* 'Artist
      '(static IFoo
         (foo [_])
         (bar [_])
         static field sel '[:woz ?noz]
         Object
         (render [_ {:keys [self artists]}]
           (om.dom/div nil "Hello!"))
         (componentWillUnmount [this]
           (first [1 2 3])))))

  (pprint
    (defui* 'Artist
      '(Object
        (initLocalState [this]
          {:foo 'bar})
        (render [_ {:keys [self artists]}]
          (om.dom/div nil "Hello!")))))

  (pprint
    (defui* 'Component
     '(static om.next/IQuery
        (query [this] '[:foo/bar :baz/woz]))))
  )
