(ns om.devcards.process-roots-tests
  (:require-macros [devcards.core :refer [defcard deftest]])
  (:require [cljs.test :refer-macros [is async]]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))

(deftest process-roots-tests
         "Process roots supports recursive queries with a query-root on join"
         (let [query-fragment (with-meta {:people [:person/name]} {:query-root true})
               full-query [{:widget [query-fragment]}]
               sample-response {:people [{:person/name "Joe"}]}
               {:keys [query rewrite] :as result}
               (try
                 (om/process-roots full-query)
                 (catch js/Error e (str "Process Failed" e)))
               ]
           (is (not (string? result)) (str "Query to send:" query))
           (when rewrite
             (is (= [query-fragment] query))
             (is (= {:widget sample-response} (rewrite sample-response)))))
         "Process roots works for queries that root normally"
         (let [full-query [{:widget [{:people [:person/name]}]}]
               sample-response {:widget {:people [{:person/name "Joe"}]}}
               {:keys [query rewrite] :as result}
               (try
                 (om/process-roots full-query)
                 (catch js/Error e (str "Process Failed" e)))
               ]
           (is (not (string? result)) (str "Query to send:" query))
           (when rewrite
             (is (= full-query query))
             (is (= sample-response (rewrite sample-response)))))
         "Process roots works for queries that root normally and contain recursive ..."
         (let [full-query [{:widget [{:people [:person/name {:person/mate '...}]}]}]
               sample-response {:widget {:people [{:person/name "Joe"}]}}
               {:keys [query rewrite] :as result}
               (try
                 (om/process-roots full-query)
                 (catch js/Error e (str "Process Failed" e)))
               ]
           (is (not (string? result)) (str "Query to send:" query))
           (when rewrite
             (is (= full-query query))
             (is (= sample-response (rewrite sample-response)))))
         )


