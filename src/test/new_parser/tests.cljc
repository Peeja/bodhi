(ns new-parser.tests
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [clojure.set :as set]
            [clojure.spec :as s #?@(:cljs [:include-macros true])]
            [com.gfredericks.test.chuck.clojure-test #?(:clj :refer :cljs :refer-macros) [checking]]
            [new-parser.core :as new-parser]
            [new-parser.om-specs :as om-specs]
            [om.next :as om]
            [om.util :as om-util]))

(deftest basic-parser-reads-from-state
  (testing "Requires an outer-parser."
    (is (thrown-with-msg? AssertionError #":outer-parser"
                          ((new-parser/basic-parser) {} [:some-key]))))

  (let [parser (new-parser/composed-parser (new-parser/basic-parser))
        state (atom {:other-info {:some "data"
                                  :and "more data"}
                     :current-user [:user/by-id 123]
                     :user/by-id {123 {:user/favorite-color :color/blue
                                       :user/favorite-number 42
                                       :user/favorite-fellow-user [:user/by-id 456]}
                                  456 {:user/favorite-color :color/red
                                       :user/favorite-number 7}}})]

    (testing "With no join, reads everything."
      (is (= {:other-info {:some "data"
                           :and "more data"}}
             (parser {:state state} '[:other-info]))))

    (testing "With a joined query, reads that."
      (is (= {:other-info {:some "data"}}
             (parser {:state state} '[{:other-info [:some]}]))))

    (testing "Reads through an ident."
      (is (= {:current-user {:user/favorite-color :color/blue}}
             (parser {:state state} '[{:current-user [:user/favorite-color]}]))))

    (testing "With [*], reads everything."
      (is (= {:current-user {:user/favorite-color :color/blue
                             :user/favorite-number 42
                             :user/favorite-fellow-user {:user/favorite-color :color/red
                                                         :user/favorite-number 7}}}
             (parser {:state state} '[{:current-user [*]}]))))

    (testing "Reads a singleton ident."
      (is (= {'[:current-user _] {:user/favorite-color :color/blue}}
             (parser {:state state} '[{[:current-user _] [:user/favorite-color]}]))))

    (testing "Reads a non-singleton ident"
      (is (= {[:user/by-id 123] {:user/favorite-color :color/blue}}
             (parser {:state state} '[{[:user/by-id 123] [:user/favorite-color]}]))))

    (testing "Reads a singleton ident from anywhere."
      (is (= {:other-info {'[:current-user _] {:user/favorite-color :color/blue}}}
             (parser {:state state} '[{:other-info [{[:current-user _] [:user/favorite-color]}]}]))))

    (testing "Reads a non-singleton ident from anywhere."
      (is (= {:other-info {[:user/by-id 123] {:user/favorite-color :color/blue}}}
             (parser {:state state} '[{:other-info [{[:user/by-id 123] [:user/favorite-color]}]}]))))

    (testing "Reads through multiple nodes."
      (is (= {:current-user {:user/favorite-fellow-user {:user/favorite-color :color/red}}}
             (parser {:state state} '[{:current-user [{:user/favorite-fellow-user [:user/favorite-color]}]}]))))

    (testing "Reads recursively."
      (is (= {:current-user {:user/favorite-color :color/blue
                             :user/favorite-fellow-user {:user/favorite-color :color/red}}}
             (parser {:state state} '[{:current-user [:user/favorite-color
                                                      {:user/favorite-fellow-user ...}]}]))))

    (testing "Passes on remote queries."
      (is (= '[{:current-user [:user/favorite-color
                               {:user/favorite-fellow-user ...}]}]
             (parser {:state state} '[{:current-user [:user/favorite-color
                                                      {:user/favorite-fellow-user ...}]}]
                     :some-remote))))))

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
      (is (= (inner-parser {} [aliased-from] nil)
             (parser {} [aliased-from] nil)))
      (is (= (inner-parser {} [{aliased-from joined-query}] nil)
             (parser {} [{aliased-from joined-query}] nil)))
      (is (= (inner-parser {} [{(list aliased-from params) joined-query}] nil)
             (parser {} [{(list aliased-from params) joined-query}] nil)))

      ;; (The params can appear in multiple places in the query syntax, so we
      ;; convert the results to AST form where it's unambiguous before comparing.)
      (is (= (om/query->ast (inner-parser {} [aliased-from] :remote))
             (om/query->ast (parser {} [aliased-from] :remote))))
      (is (= (om/query->ast (inner-parser {} [{aliased-from joined-query}] :remote))
             (om/query->ast (parser {} [{aliased-from joined-query}] :remote))))
      (is (= (om/query->ast (inner-parser {} [{(list aliased-from params) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-from params) joined-query}] :remote))))


      ;; With a :< param, it should alias the key and pass on the remaining params.
      (is (= (set/rename-keys (inner-parser {} [aliased-from] nil)
                              {aliased-from aliased-to})
             (parser {} [(list aliased-to {:< aliased-from})] nil)))
      (is (= (set/rename-keys (inner-parser {} [{aliased-from joined-query}] nil)
                              {aliased-from aliased-to})
             (parser {} [{(list aliased-to {:< aliased-from}) joined-query}] nil)))
      (is (= (set/rename-keys (inner-parser {} [{(list aliased-from params) joined-query}] nil)
                              {aliased-from aliased-to})
             (parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] nil)))

      ;; But for remote queries, it should pass the :< along to the remote.
      (is (= (om/query->ast (inner-parser {} [(list aliased-to {:< aliased-from})] :remote))
             (om/query->ast (parser {} [(list aliased-to {:< aliased-from})] :remote))))
      (is (= (om/query->ast (inner-parser {} [{(list aliased-to {:< aliased-from}) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-to {:< aliased-from}) joined-query}] :remote))))
      (is (= (om/query->ast (inner-parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] :remote)))))))

(deftest param-indexed-parser
  (testing "Requires an outer-parser."
    (is (thrown-with-msg? AssertionError #":outer-parser"
                          ((new-parser/param-indexed-parser (om/parser {}))
                           {} [:some-key]))))

  (let [parser ((comp new-parser/composed-parser
                      new-parser/param-indexed-parser
                      new-parser/basic-parser))
        state (atom {:current-user [:user/by-id 123]
                     :user/by-id {123 {:user/favorite-color :color/blue
                                       :user/pet {{:pet/name "Milo"} {:pet/name "Milo"
                                                                      :pet/species :pet-species/cat
                                                                      :pet/description "orange tabby"}
                                                  {:pet/name "Otis"} [:pet/by-id 654]}}}
                     :pet/by-id {654 {:pet/name "Otis"
                                      :pet/species :pet-species/dog
                                      :pet/description "pug"}}})]

    (testing "Fetches from a param-indexed key."
      (is (= {:current-user {:user/favorite-color :color/blue
                             :user/pet {:pet/name "Milo"
                                        :pet/species :pet-species/cat}}}
             (parser {:state state} '[{:current-user
                                       [:user/favorite-color
                                        {(:user/pet {:pet/name "Milo"})
                                         [:pet/name :pet/species]}]}]))))

    (testing "Fetches from a param-indexed key through an ident."
      (is (= {:current-user {:user/favorite-color :color/blue
                             :user/pet {:pet/name "Otis"
                                        :pet/species :pet-species/dog}}}
             (parser {:state state} '[{:current-user
                                       [:user/favorite-color
                                        {(:user/pet {:pet/name "Otis"})
                                         [:pet/name :pet/species]}]}]))))

    (testing "Passes on remote queries."
      (is (= '[{:current-user
                [:user/favorite-color
                 {(:user/pet {:pet/name "Milo"})
                  [:pet/name :pet/species]}]}]
             (parser {:state state} '[{:current-user
                                       [:user/favorite-color
                                        {(:user/pet {:pet/name "Milo"})
                                         [:pet/name :pet/species]}]}]
                     :some-remote))))))

(deftest all-together-now
  (let [parser ((comp new-parser/composed-parser
                      new-parser/aliasing-parser
                      new-parser/param-indexed-parser
                      new-parser/basic-parser))
        state (atom {:other-info {:some "data"
                                  :and "more data"}
                     :current-user [:user/by-id 123]
                     :user/by-id {123 {:user/favorite-color :color/blue
                                       :user/favorite-number 42
                                       :user/favorite-fellow-user [:user/by-id 456]
                                       :user/pet {{:pet/name "Milo"} [:pet/by-id 321]
                                                  {:pet/name "Otis"} [:pet/by-id 654]}}
                                  456 {:user/favorite-color :color/red
                                       :user/favorite-number 7}}
                     :pet/by-id {321 {:pet/name "Milo"
                                      :pet/species :pet-species/cat
                                      :pet/description "orange tabby"}
                                 654 {:pet/name "Otis"
                                      :pet/species :pet-species/dog
                                      :pet/description "pug"}}})]

    (is (= {:current-user-1 {:user/favorite-color :color/blue
                             :milo {:pet/name "Milo"
                                    :pet/species :pet-species/cat}
                             :otis {:pet/name "Otis"
                                    :pet/species :pet-species/dog}}
            :current-user-2 {:user/favorite-number 42
                             :favorite-user-1 {:user/favorite-color :color/red}
                             :favorite-user-2 {:user/favorite-number 7}}}
           (parser {:state state} '[{(:current-user-1 {:< :current-user})
                                     [:user/favorite-color
                                      {(:milo {:< :user/pet :pet/name "Milo"})
                                       [:pet/name :pet/species]}
                                      {(:otis {:< :user/pet :pet/name "Otis"})
                                       [:pet/name :pet/species]}]}
                                    {(:current-user-2 {:< :current-user})
                                     [:user/favorite-number
                                      {(:favorite-user-1 {:< :user/favorite-fellow-user})
                                       [:user/favorite-color
                                        {:user/favorite-fellow-user ...}]}
                                      {(:favorite-user-2 {:< :user/favorite-fellow-user})
                                       [:user/favorite-number
                                        {:user/favorite-fellow-user ...}]}]}])))))
