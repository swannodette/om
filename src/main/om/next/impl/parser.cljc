(ns om.next.impl.parser)

(declare ->ast)

(defn symbol->ast [k]
  {:dkey k
   :key  k})

(defn keyword->ast [k]
  {:type :prop
   :dkey k
   :key  k})

(defn call->ast [[f args]]
  (if (= 'quote f)
    {:type :remote}
    (let [ast (update-in (->ast f) [:params] merge (or args {}))]
     (cond-> ast
       (symbol? (:dkey ast)) (assoc :type :call)))))

(defn join->ast [join]
  (let [[k v] (first join)
        ast   (->ast k)
        ref?  (vector? (:key ast))]
    (assoc ast :type :prop :sel v)))

(defn ref->ast [[k id :as ref]]
  {:type   :prop
   :dkey   k
   :key    ref
   :params {:id id}})

(defn ->ast [x]
  (cond
    (symbol? x)  (symbol->ast x)
    (keyword? x) (keyword->ast x)
    (map? x)     (join->ast x)
    (vector? x)  (ref->ast x)
    (seq? x)     (call->ast x)
    :else        (throw
                   (ex-info (str "Invalid expression " x)
                     {:type :error/invalid-expression}))))

(defn path-meta [v path]
  (let [v' (cond->> v
             (vector? v) (into [] (map-indexed #(path-meta %2 (conj path %1)))))]
    (cond-> v'
            #?(:cljs (satisfies? IWithMeta v') :clj (instance? clojure.lang.IMeta v')) (vary-meta assoc :om-path path))))

(defn parser [{:keys [read mutate]}]
  (fn self
    ([env sel] (self env sel false))
    ([env sel #?@(:clj [remote?] :cljs [^boolean remote?])]
     (let [{:keys [path] :as env}
           (cond-> (assoc env :parse self)
             (not (contains? env :path)) (assoc :path [])
             remote? (assoc :remote true))]
       (letfn [(step [ret expr]
                 (let [{:keys [key dkey params sel] :as ast} (->ast expr)
                       env   (cond-> env
                               (not (nil? sel)) (assoc :selector sel))
                       type  (:type ast)
                       call? (= :call type)
                       res   (case type
                               :call   (mutate env dkey params)
                               :prop   (read env dkey params)
                               :remote nil)]
                   (if remote?
                     (cond-> ret
                       (true? (:remote res)) (conj expr)
                       (= :remote type)     (conj (second expr)))
                     (if-not (or call? (not= :remote type) (contains? res :value))
                       ret
                       (let [error (atom nil)]
                         (when (and call? (not (nil? (:action res))))
                           (try
                             ((:action res))
                             #?(:clj  (catch Throwable e
                                        (reset! error e))
                                :cljs (catch :default e
                                        (reset! error e)))))
                         (let [value (:value res)]
                           (cond-> ret
                             @error (assoc key @error)
                             (not (nil? value)) (assoc key (path-meta value (conj path key))))))))))]
         (reduce step (if-not remote? {} []) sel))))))

(defn dispatch [_ k _] k)
