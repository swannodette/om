(ns om.dom
  (:refer-clojure :exclude [map meta time])
  #?(:clj
     (:require [clojure.string :as str]
               [om.next.protocols :as p]
               [clojure.core.reducers :as r]
               [om.checksums :as chk])))

(def tags
  '[a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    big
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    data
    datalist
    dd
    del
    details
    dfn
    dialog
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h1
    h2
    h3
    h4
    h5
    h6
    head
    header
    hr
    html
    i
    iframe
    img
    ins
    kbd
    keygen
    label
    legend
    li
    link
    main
    map
    mark
    menu
    menuitem
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    output
    p
    param
    picture
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    script
    section
    small
    source
    span
    strong
    style
    sub
    summary
    sup
    table
    tbody
    td
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr

    ;; svg
    circle
    clipPath
    ellipse
    g
    line
    mask
    path
    pattern
    polyline
    rect
    svg
    text
    defs
    linearGradient
    polygon
    radialGradient
    stop
    tspan])

(defn ^:private gen-react-dom-inline-fn [tag]
  `(defmacro ~tag [opts# & children#]
     `(~'~(symbol "js" (str "React.DOM." (name tag))) ~opts#
        ~@(clojure.core/map (fn [x#] `(om.util/force-children ~x#)) children#))))

(defmacro ^:private gen-react-dom-inline-fns []
  (when (boolean (:ns &env))
    `(do
       ~@(clojure.core/map gen-react-dom-inline-fn tags))))

#?(:clj (gen-react-dom-inline-fns))

(defn ^:private gen-react-dom-fn [tag]
  `(defn ~tag [opts# & children#]
     (.apply ~(symbol "js" (str "React.DOM." (name tag))) nil
       (cljs.core/into-array
         (cons opts# (cljs.core/map om.util/force-children children#))))))

(defmacro ^:private gen-react-dom-fns []
  `(do
     ~@(clojure.core/map gen-react-dom-fn tags)))

;; ===================================================================
;; Server-side rendering

;; https://github.com/facebook/react/blob/57ae3b/src/renderers/dom/shared/SVGDOMPropertyConfig.js
;; https://github.com/facebook/react/blob/57ae3b/src/renderers/dom/shared/HTMLDOMPropertyConfig.js
#?(:clj
   (def supported-attrs
     #{ ;; HTML
       "accept" "acceptCharset" "accessKey" "action" "allowFullScreen" "allowTransparency" "alt"
       "async" "autoComplete" "autoFocus" "autoPlay" "capture" "cellPadding" "cellSpacing" "challenge"
       "charSet" "checked" "cite" "classID" "className" "colSpan" "cols" "content" "contentEditable"
       "contextMenu" "controls" "coords" "crossOrigin" "data" "dateTime" "default" "defer" "dir"
       "disabled" "download" "draggable" "encType" "form" "formAction" "formEncType" "formMethod"
       "formNoValidate" "formTarget" "frameBorder" "headers" "height" "hidden" "high" "href" "hrefLang"
       "htmlFor" "httpEquiv" "icon" "id" "inputMode" "integrity" "is" "keyParams" "keyType" "kind" "label"
       "lang" "list" "loop" "low" "manifest" "marginHeight" "marginWidth" "max" "maxLength" "media"
       "mediaGroup" "method" "min" "minLength" "multiple" "muted" "name" "noValidate" "nonce" "open"
       "optimum" "pattern" "placeholder" "poster" "preload" "profile" "radioGroup" "readOnly" "referrerPolicy"
       "rel" "required" "reversed" "role" "rowSpan" "rows" "sandbox" "scope" "scoped" "scrolling" "seamless" "selected"
       "shape" "size" "sizes" "span" "spellCheck" "src" "srcDoc" "srcLang" "srcSet" "start" "step" "style" "summary"
       "tabIndex" "target" "title" "type" "useMap" "value" "width" "wmode" "wrap"
       ;; RDF
       "about" "datatype" "inlist" "prefix" "property" "resource" "typeof" "vocab"
       ;; SVG
       "accentHeight" "accumulate" "additive" "alignmentBaseline" "allowReorder" "alphabetic"
       "amplitude" "ascent" "attributeName" "attributeType" "autoReverse" "azimuth"
       "baseFrequency" "baseProfile" "bbox" "begin" "bias" "by" "calcMode" "clip"
       "clipPathUnits" "contentScriptType" "contentStyleType" "cursor" "cx" "cy" "d"
       "decelerate" "descent" "diffuseConstant" "direction" "display" "divisor" "dur"
       "dx" "dy" "edgeMode" "elevation" "end" "exponent" "externalResourcesRequired"
       "fill" "filter" "filterRes" "filterUnits" "focusable" "format" "from" "fx" "fy"
       "g1" "g2" "glyphRef" "gradientTransform" "gradientUnits" "hanging" "ideographic"
       "in" "in2" "intercept" "k" "k1" "k2" "k3" "k4" "kernelMatrix" "kernelUnitLength"
       "kerning" "keyPoints" "keySplines" "keyTimes" "lengthAdjust" "limitingConeAngle"
       "local" "markerHeight" "markerUnits" "markerWidth" "mask" "maskContentUnits"
       "maskUnits" "mathematical" "mode" "numOctaves" "offset" "opacity" "operator"
       "order" "orient" "orientation" "origin" "overflow" "pathLength" "patternContentUnits"
       "patternTransform" "patternUnits" "points" "pointsAtX" "pointsAtY" "pointsAtZ"
       "preserveAlpha" "preserveAspectRatio" "primitiveUnits" "r" "radius" "refX" "refY"
       "repeatCount" "repeatDur" "requiredExtensions" "requiredFeatures" "restart"
       "result" "rotate" "rx" "ry" "scale" "seed" "slope" "spacing" "specularConstant"
       "specularExponent" "speed" "spreadMethod" "startOffset" "stdDeviation" "stemh"
       "stemv" "stitchTiles" "string" "stroke" "surfaceScale" "systemLanguage" "tableValues"
       "targetX" "targetY" "textLength" "to" "transform" "u1" "u2" "unicode" "values"
       "version" "viewBox" "viewTarget" "visibility" "widths" "x" "x1" "x2" "xChannelSelector"
       "xmlns" "y" "y1" "y2" "yChannelSelector" "z" "zoomAndPan" "arabicForm" "baselineShift"
       "capHeight" "clipPath" "clipRule" "colorInterpolation" "colorInterpolationFilters"
       "colorProfile" "colorRendering" "dominantBaseline" "enableBackground" "fillOpacity"
       "fillRule" "floodColor" "floodOpacity" "fontFamily" "fontSize" "fontSizeAdjust"
       "fontStretch" "fontStyle" "fontVariant" "fontWeight" "glyphName" "glyphOrientationHorizontal"
       "glyphOrientationVertical" "horizAdvX" "horizOriginX" "imageRendering" "letterSpacing"
       "lightingColor" "markerEnd" "markerMid" "markerStart" "overlinePosition" "overlineThickness"
       "paintOrder" "panose1" "pointerEvents" "renderingIntent" "shapeRendering" "stopColor"
       "stopOpacity" "strikethroughPosition" "strikethroughThickness" "strokeDasharray"
       "strokeDashoffset" "strokeLinecap" "strokeLinejoin" "strokeMiterlimit" "strokeOpacity"
       "strokeWidth" "textAnchor" "textDecoration" "textRendering" "underlinePosition"
       "underlineThickness" "unicodeBidi" "unicodeRange" "unitsPerEm" "vAlphabetic"
       "vHanging" "vIdeographic" "vMathematical" "vectorEffect" "vertAdvY" "vertOriginX"
       "vertOriginY" "wordSpacing" "writingMode" "xHeight"

       "xlinkActuate" "xlinkArcrole" "xlinkHref" "xlinkRole" "xlinkShow" "xlinkTitle"
       "xlinkType" "xmlBase" "xmlnsXlink" "xmlLang" "xmlSpace"

       ;; Non-standard Properties
       "autoCapitalize" "autoCorrect" "autoSave" "color" "itemProp" "itemScope"
       "itemType" "itemID" "itemRef" "results" "security" "unselectable"

       ;; Special case
       "data-reactid" "data-reactroot"}))

#?(:clj
   (def no-suffix
     #{"animationIterationCount" "boxFlex" "boxFlexGroup" "boxOrdinalGroup"
       "columnCount" "fillOpacity" "flex" "flexGrow" "flexPositive" "flexShrink"
       "flexNegative" "flexOrder" "fontWeight" "lineClamp" "lineHeight" "opacity"
       "order" "orphans" "stopOpacity" "strokeDashoffset" "strokeOpacity"
       "strokeWidth" "tabSize" "widows" "zIndex" "zoom"}))

#?(:clj
   (def lower-case-attrs
     #{"accessKey" "allowFullScreen" "allowTransparency" "as" "autoComplete"
       "autoFocus" "autoPlay" "contentEditable" "contextMenu" "crossOrigin"
       "cellPadding" "cellSpacing" "charSet" "classID" "colSpan" "dateTime"
       "encType" "formAction" "formEncType" "formMethod" "formNoValidate"
       "formTarget" "frameBorder" "hrefLang" "inputMode" "keyParams"
       "keyType" "marginHeight" "marginWidth" "maxLength" "mediaGroup"
       "minLength" "noValidate" "playsInline" "radioGroup" "readOnly" "rowSpan"
       "spellCheck" "srcDoc" "srcLang" "srcSet" "tabIndex" "useMap"
       "autoCapitalize" "autoCorrect" "autoSave" "itemProp" "itemScope"
       "itemType" "itemID" "itemRef"}))

#?(:clj
   (def kebab-case-attrs
     #{"acceptCharset" "httpEquiv" "accentHeight" "alignmentBaseline" "arabicForm"
       "baselineShift" "capHeight" "clipPath" "clipRule" "colorInterpolation"
       "colorInterpolationFilters" "colorProfile" "colorRendering" "dominantBaseline"
       "enableBackground" "fillOpacity" "fillRule" "floodColor" "floodOpacity"
       "fontFamily" "fontSize" "fontSizeAdjust" "fontStretch" "fontStyle"
       "fontVariant" "fontWeight" "glyphName" "glyphOrientationHorizontal"
       "glyphOrientationVertical" "horizAdvX" "horizOriginX" "imageRendering"
       "letterSpacing" "lightingColor" "markerEnd" "markerMid" "markerStart"
       "overlinePosition" "overlineThickness" "paintOrder" "panose1" "pointerEvents"
       "renderingIntent" "shapeRendering" "stopColor" "stopOpacity" "strikethroughPosition"
       "strikethroughThickness" "strokeDasharray" "strokeDashoffset" "strokeLinecap"
       "strokeLinejoin" "strokeMiterlimit" "strokeOpacity" "strokeWidth" "textAnchor"
       "textDecoration" "textRendering" "underlinePosition" "underlineThickness"
       "unicodeBidi" "unicodeRange" "unitsPerEm" "vAlphabetic" "vHanging" "vIdeographic"
       "vMathematical" "vectorEffect" "vertAdvY" "vertOriginX" "vertOriginY" "wordSpacing"
       "writingMode" "xHeight"}))

#?(:clj
   (def colon-between-attrs
     #{"xlinkActuate" "xlinkArcrole" "xlinkHref" "xlinkRole" "xlinkShow" "xlinkTitle"
       "xlinkType" "xmlBase" "xmlnsXlink" "xmlLang" "xmlSpace"}))

#?(:clj (declare render-element!))

#?(:clj
   (defn append!
     ([^StringBuilder sb s0] (.append sb s0))
     ([^StringBuilder sb s0 s1]
      (.append sb s0)
      (.append sb s1))
     ([^StringBuilder sb s0 s1 s2]
      (.append sb s0)
      (.append sb s1)
      (.append sb s2))
     ([^StringBuilder sb s0 s1 s2 s3]
      (.append sb s0)
      (.append sb s1)
      (.append sb s2)
      (.append sb s3))
     ([^StringBuilder sb s0 s1 s2 s3 s4]
      (.append sb s0)
      (.append sb s1)
      (.append sb s2)
      (.append sb s3)
      (.append sb s4))
     ([^StringBuilder sb s0 s1 s2 s3 s4 & rest]
      (.append sb s0)
      (.append sb s1)
      (.append sb s2)
      (.append sb s3)
      (.append sb s4)
      (doseq [s rest]
        (.append sb s)))))

#?(:clj
   (defn escape-html ^String [^String s]
     (let [len (count s)]
       (loop [^StringBuilder sb nil
              i                 (int 0)]
         (if (< i len)
           (let [char (.charAt s i)
                 repl (case char
                        \& "&amp;"
                        \< "&lt;"
                        \> "&gt;"
                        \" "&quot;"
                        \' "&#x27;"
                        nil)]
             (if (nil? repl)
               (if (nil? sb)
                 (recur nil (inc i))
                 (recur (doto sb
                          (.append char))
                   (inc i)))
               (if (nil? sb)
                 (recur (doto (StringBuilder.)
                          (.append s 0 i)
                          (.append repl))
                   (inc i))
                 (recur (doto sb
                          (.append repl))
                   (inc i)))))
           (if (nil? sb) s (str sb)))))))

#?(:clj
   (defrecord Element [tag attrs react-key children]
     p/IReactDOMElement
     (-render-to-string [this react-id sb]
       (render-element! this react-id sb))))

#?(:clj
   (defrecord Text [s]
     p/IReactDOMElement
     (-render-to-string [this react-id sb]
       (assert (string? s))
       (append! sb (escape-html s)))))

#?(:clj
   (defrecord ReactText [text]
     p/IReactDOMElement
     (-render-to-string [this react-id sb]
       (assert (string? text))
       (append! sb "<!-- react-text: " @react-id " -->" (escape-html text) "<!-- /react-text -->")
       (vswap! react-id inc))))

#?(:clj
   (defrecord ReactEmpty []
     p/IReactDOMElement
     (-render-to-string [this react-id sb]
       (append! sb "<!-- react-empty: " @react-id " -->")
       (vswap! react-id inc))))

#?(:clj
   (defn text-node
     "HTML text node"
     [s]
     (map->Text {:s s})))

#?(:clj
   (defn react-text-node
     "HTML text node"
     [s]
     (map->ReactText {:text s})))

#?(:clj
   (defn- react-empty-node []
     (map->ReactEmpty {})))

#?(:clj
   (defn- render-component [c]
     (if (or (nil? c)
             (instance? om.next.protocols.IReactDOMElement c)
             (satisfies? p/IReactDOMElement c))
       c
       (recur (p/-render c)))))

#?(:clj
   (defn element
     "Creates a dom node."
     [{:keys [tag attrs react-key children] :as elem}]
     (assert (name tag))
     (assert (or (nil? attrs) (map? attrs)) (format "elem %s attrs invalid" elem))
     (let [children (flatten children)
           child-node-count (count children)
           reduce-fn (if (> child-node-count 1)
                       r/reduce
                       reduce)
           children (reduce-fn
                      (fn [res c]
                        (let [c' (cond
                                   (or (instance? om.next.protocols.IReactDOMElement c)
                                       (satisfies? p/IReactDOMElement c))
                                   c

                                   (or (instance? om.next.protocols.IReactComponent c)
                                       (satisfies? p/IReactComponent c))
                                   (let [rendered (if-let [element (render-component c)]
                                                    element
                                                    (react-empty-node))]
                                     (assoc rendered :react-key
                                       (some-> (:props c) :omcljs$reactKey)))

                                   (or (string? c) (number? c))
                                   (let [c (cond-> c (number? c) str)]
                                     (if (> child-node-count 1)
                                       (react-text-node c)
                                       (text-node c)))

                                   (nil? c) nil

                                   :else
                                   (throw (IllegalArgumentException. (str "Invalid child element: ") c)))]
                          (cond-> res
                            (some? c') (conj c'))))
                      [] children)]
       (map->Element {:tag (name tag)
                      :attrs attrs
                      :react-key react-key
                      :children children}))))

#?(:clj
   (defn camel->other-case [^String sep]
     (fn ^String [^String s]
       (-> s
         (str/replace #"([A-Z0-9])" (str sep "$1"))
         str/lower-case))))

#?(:clj
   (def camel->kebab-case
     (camel->other-case "-")))

#?(:clj
   (def camel->colon-between
     (camel->other-case ":")))

#?(:clj
   (defn coerce-attr-key ^String [^String k]
     (cond
       (contains? lower-case-attrs k) (str/lower-case k)
       (contains? kebab-case-attrs k) (camel->kebab-case k)
       ;; special cases
       (= k "className") "class"
       (= k "htmlFor") "for"
       (contains? colon-between-attrs k) (camel->colon-between k)
       :else k)))

#?(:clj
   (defn render-xml-attribute! [sb name value]
     (let [name (coerce-attr-key (clojure.core/name name))]
       (append! sb " " name "=\""
         (cond-> value
           (string? value) escape-html) "\""))))

#?(:clj
   (defn normalize-styles! [sb styles]
     (letfn [(coerce-value [k v]
               (cond-> v
                 (string? v)
                 escape-html
                 (and (number? v)
                   (not (contains? no-suffix k))
                   (pos? v))
                 (str "px")))]
       (run! (fn [[k v]]
               (let [k (name k)]
                 (append! sb (camel->kebab-case k) ":" (coerce-value k v) ";")))
         styles))))

#?(:clj
   (defn render-styles! [sb styles]
     (when-not (empty? styles)
       (append! sb " style=\"")
       (normalize-styles! sb styles)
       (append! sb "\""))))

#?(:clj
   (defn render-attribute! [sb [key value]]
     (cond
       (or (fn? value)
         (not value))
       nil

       (identical? key :style)
       (render-styles! sb value)

       ;; TODO: not sure if we want to limit values to strings/numbers - António
       (and (or (contains? supported-attrs (name key))
                (.startsWith (name key) "data-"))
            (or (true? value) (string? value) (number? value)))
       (if (true? value)
         (append! sb " " (coerce-attr-key (name key)))
         (render-xml-attribute! sb key value))

       :else nil)))

;; some props assigned first in input and option. see:
;; https://github.com/facebook/react/blob/680685/src/renderers/dom/client/wrappers/ReactDOMOption.js#L108
;; https://github.com/facebook/react/blob/680685/src/renderers/dom/client/wrappers/ReactDOMInput.js#L63
#?(:clj
   (defn render-attr-map! [sb tag attrs]
     (letfn [(sorter [order]
               (fn [[k _]]
                 (get order k (->> (vals order)
                                (apply max)
                                inc))))]
       (let [attrs (cond->> attrs
                     (= tag "input") (sort-by (sorter {:type 0 :step 1
                                                       :min 2 :max 3}))
                     (= tag "option") (sort-by (sorter {:selected 0})))]
         (run! (partial render-attribute! sb) attrs)))))

#?(:clj
   (def ^{:doc "A list of elements that must be rendered without a closing tag."
          :private true}
     void-tags
     #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link"
       "meta" "param" "source" "track" "wbr"}))

#?(:clj
   (defn render-unescaped-html! [sb m]
     (if-not (contains? m :__html)
       (throw (IllegalArgumentException. "`props.dangerouslySetInnerHTML` must be in the form `{:__html ...}`")))
     (when-let [html (:__html m)]
       (append! sb html))))

#?(:clj
   (defn container-tag?
     "Returns true if the tag has content or is not a void tag. In non-HTML modes,
      all contentless tags are assumed to be void tags."
     [tag content]
     (or content (and (not (void-tags tag))))))

#?(:clj
   (defn render-element!
     "Render a tag vector as a HTML element string."
     [{:keys [tag attrs children]} react-id ^StringBuilder sb]
     (append! sb "<" tag)
     (render-attr-map! sb tag attrs)
     (let [react-id-val @react-id]
       (when (= react-id-val 1)
         (append! sb " data-reactroot=\"\""))
       (append! sb " data-reactid=\"" react-id-val "\"")
       (vswap! react-id inc))
     (if (container-tag? tag (seq children))
       (do
         (append! sb ">")
         (if-let [html-map (:dangerouslySetInnerHTML attrs)]
           (render-unescaped-html! sb html-map)
           (run! #(p/-render-to-string % react-id sb) children))
         (append! sb "</" tag ">"))
       (append! sb "/>"))))

#?(:clj
   (defn gen-tag-fn [tag]
     `(defn ~tag [~'attrs & ~'children]
        (element {:tag (quote ~tag)
                  :attrs (dissoc ~'attrs :ref :key)
                  :react-key (:key ~'attrs)
                  :children ~'children}))))

#?(:clj
   (defmacro gen-all-tags []
     (when-not (boolean (:ns &env))
       `(do
          ~@(clojure.core/map gen-tag-fn
              ;; In CLJS we generate these separately
              (into tags '[input textarea option select]))))))

#?(:clj (gen-all-tags))

#?(:clj
   (def key-escape-lookup
     {"=" "=0"
      ":" "=2"}))

;; preserves testability without having to compute checksums
#?(:clj
   (defn- render-to-str* ^StringBuilder [x]
     {:pre [(or (instance? om.next.protocols.IReactComponent x)
                (instance? om.next.protocols.IReactDOMElement x)
                (satisfies? p/IReactComponent x)
                (satisfies? p/IReactDOMElement x))]}
     (let [element (if-let [element (cond-> x
                                      (or (instance? om.next.protocols.IReactComponent x)
                                          (satisfies? p/IReactComponent x))
                                      render-component)]
                     element
                     (react-empty-node))
           sb (StringBuilder.)]
       (p/-render-to-string element (volatile! 1) sb)
       sb)))

#?(:clj
   (defn render-to-str ^String [x]
     (let [sb (render-to-str* x)]
       (chk/assign-react-checksum sb)
       (str sb))))

#?(:clj
   (defn node
     "Returns the dom node associated with a component's React ref."
     ([component]
      {:pre [(or (instance? om.next.protocols.IReactComponent component)
                 (satisfies? p/IReactComponent component))]}
      (p/-render component))
     ([component name]
      {:pre [(or (instance? om.next.protocols.IReactComponent component)
                 (satisfies? p/IReactComponent component))]}
      (some-> @(:refs component) (get name) p/-render))))

#?(:clj
   (defn create-element
     "Create a DOM element for which there exists no corresponding function.
      Useful to create DOM elements not included in React.DOM. Equivalent
      to calling `js/React.createElement`"
     ([tag]
      (create-element tag nil))
     ([tag opts & children]
      (element {:tag tag
                :attrs (dissoc opts :ref :key)
                :react-key (:key opts)
                :children children}))))
