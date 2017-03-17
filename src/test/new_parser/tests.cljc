(ns new-parser.tests
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.set :as set]
            [clojure.spec :as s #?@(:cljs [:include-macros true])]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking]]
            [new-parser.core :as new-parser]
            [new-parser.om-specs :as om-specs]
            [om.next :as om]
            [om.util :as om-util]))

(deftest aliasing-parser-aliases
  (let [inner-read (fn [{:keys [query ast parser] {:keys [key]} :ast :as env} dispatch-key params]
                     {:value
                      {:key key
                       :params params
                       :query
                       ;; If it's a recursive join...
                       (if (or (number? query)
                               (some #(and (om-util/join? %) (= '... (om-util/join-value %))) query))
                         ;; ...we can't use the query for comparison in the
                         ;; tests below, because it will include the alias.
                         ;; Instead, put a constant value here so recursive
                         ;; cases always match.
                         :recursive-query
                         query)}

                      :remote true})
        inner-parser (om/parser {:read inner-read})
        parser (new-parser/aliasing-parser inner-parser)]

    (checking "aliasing parser aliases keys" 10
      [aliased-to gen/keyword-ns
       aliased-from (s/gen ::om-specs/join-key)
       params (s/gen ::om-specs/params)
       joined-query (s/gen ::om-specs/joined-query)]

      ;; Without a :< param, the aliasing parser should behave just like the parser it wraps.
      (is (= (inner-parser {} [{aliased-from joined-query}] nil)
             (parser {} [{aliased-from joined-query}] nil)))
      (is (= (inner-parser {} [{(list aliased-from params) joined-query}] nil)
             (parser {} [{(list aliased-from params) joined-query}] nil)))

      ;; (The params can appear in multiple places in the query syntax, so we
      ;; convert the results to AST form where it's unambiguous before comparing.)
      (is (= (om/query->ast (inner-parser {} [{aliased-from joined-query}] :remote))
             (om/query->ast (parser {} [{aliased-from joined-query}] :remote))))
      (is (= (om/query->ast (inner-parser {} [{(list aliased-from params) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-from params) joined-query}] :remote))))


      ;; With a :< param, it should alias the key and pass on the remaining params.
      (is (= (set/rename-keys (inner-parser {} [{aliased-from joined-query}] nil)
                              {aliased-from aliased-to})
             (parser {} [{(list aliased-to {:< aliased-from}) joined-query}] nil)))
      (is (= (set/rename-keys (inner-parser {} [{(list aliased-from params) joined-query}] nil)
                              {aliased-from aliased-to})
             (parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] nil)))

      ;; But for remote queries, it should pass the :< along to the remote.
      (is (= (om/query->ast (inner-parser {} [{(list aliased-to {:< aliased-from}) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-to {:< aliased-from}) joined-query}] :remote))))
      (is (= (om/query->ast (inner-parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] :remote)))))))
