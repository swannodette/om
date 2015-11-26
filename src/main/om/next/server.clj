(ns om.next.server
  (:require [om.next.impl.parser :as parser]
            [om.transit :as transit]))

(defn parser
  "Create a parser. The argument is a map of two keys, :read and :mutate. Both
   functions should have the signature (Env -> Key -> Params -> ParseResult)."
  [opts]
  (parser/parser (assoc opts :elide-paths true)))

(defn reader
  "Create a Om Next transit reader. This reader can handle the tempid type.
   Can pass transit reader customization opts map."
  ([in] (transit/reader in))
  ([in opts] (transit/reader in opts)))

(defn writer
  "Create a Om Next transit writer. This writer can handle the tempid type.
   Can pass transit writer customization opts map."
  ([out] (transit/writer out))
  ([out opts] (transit/writer out opts)))
