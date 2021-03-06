(ns bodhi.tests
  (:require [bodhi.aliasing :as aliasing]
            [bodhi.core :as bodhi]
            [bodhi.default-db :as default-db]
            [bodhi.om-specs :as om-specs]
            [bodhi.param-indexing :as param-indexing]
            [bodhi.query-mapping :as query-mapping]
            [cljsjs.react]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [clojure.set :as set]
            [clojure.spec :as s :include-macros true]
            [com.gfredericks.test.chuck.clojure-test :refer-macros [checking]]
            [om.next :as om :refer-macros [ui]]
            [om.util :as om-util]))

(deftest default-db-read-reads-from-state-in-db-format
  (let [parser (om/parser {:read default-db/read})
        state (atom {:other-info {:some "data"
                                  :and "more data"}
                     :app/current-user [:user/by-id 123]
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
      (is (= {:app/current-user {:user/favorite-color :color/blue}}
             (parser {:state state} '[{:app/current-user [:user/favorite-color]}]))))

    (testing "With [*], reads everything."
      (is (= {:app/current-user {:user/favorite-color :color/blue
                                 :user/favorite-number 42
                                 :user/favorite-fellow-user {:user/favorite-color :color/red
                                                             :user/favorite-number 7}}}
             (parser {:state state} '[{:app/current-user [*]}]))))

    (testing "Reads a singleton ident."
      (is (= {'[:app/current-user _] {:user/favorite-color :color/blue}}
             (parser {:state state} '[{[:app/current-user _] [:user/favorite-color]}]))))

    (testing "Reads a non-singleton ident"
      (is (= {[:user/by-id 123] {:user/favorite-color :color/blue}}
             (parser {:state state} '[{[:user/by-id 123] [:user/favorite-color]}]))))

    (testing "Reads a singleton ident from anywhere."
      (is (= {:other-info {'[:app/current-user _] {:user/favorite-color :color/blue}}}
             (parser {:state state} '[{:other-info [{[:app/current-user _] [:user/favorite-color]}]}]))))

    (testing "Reads a non-singleton ident from anywhere."
      (is (= {:other-info {[:user/by-id 123] {:user/favorite-color :color/blue}}}
             (parser {:state state} '[{:other-info [{[:user/by-id 123] [:user/favorite-color]}]}]))))

    (testing "Reads through multiple nodes."
      (is (= {:app/current-user {:user/favorite-fellow-user {:user/favorite-color :color/red}}}
             (parser {:state state} '[{:app/current-user [{:user/favorite-fellow-user [:user/favorite-color]}]}]))))

    (testing "Reads recursively."
      (is (= {:app/current-user {:user/favorite-color :color/blue
                                 :user/favorite-fellow-user {:user/favorite-color :color/red}}}
             (parser {:state state} '[{:app/current-user [:user/favorite-color
                                                          {:user/favorite-fellow-user ...}]}]))))

    (testing "Passes on remote queries."
      (is (= '[{:app/current-user [:user/favorite-color
                                   {:user/favorite-fellow-user ...}]}]
             (parser {:state state} '[{:app/current-user [:user/favorite-color
                                                          {:user/favorite-fellow-user ...}]}]
                     :some-remote))))))

(deftest aliasing-read-aliases
  (let [inner-read (fn [{:keys [query ast parser] {:keys [key]} :ast :as env} dispatch-key params]
                     {:value
                      {:key key
                       :params (not-empty params) ; Treat nil params the same as {}
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
        plain-parser (om/parser {:read inner-read})
        parser (om/parser {:read (aliasing/read inner-read)})]

    (checking "aliasing parser aliases keys" 10
      [aliased-to gen/keyword-ns
       aliased-from (s/gen ::om-specs/join-key)
       params (s/gen ::om-specs/params)
       ;; This weird such-that filters out queries that include NaN, which is
       ;; not equal to itself and makes it impossible to make sensible test
       ;; assertions.
       joined-query (gen/such-that #(= % %) (s/gen ::om-specs/joined-query))]

      ;; Without a :< param, the aliasing parser should behave just like the parser it wraps.
      (is (= (plain-parser {} [aliased-from] nil)
             (parser {} [aliased-from] nil)))
      (is (= (plain-parser {} [{aliased-from joined-query}] nil)
             (parser {} [{aliased-from joined-query}] nil)))
      (is (= (plain-parser {} [{(list aliased-from params) joined-query}] nil)
             (parser {} [{(list aliased-from params) joined-query}] nil)))

      ;; (The params can appear in multiple places in the query syntax, so we
      ;; convert the results to AST form where it's unambiguous before comparing.)
      (is (= (om/query->ast (plain-parser {} [aliased-from] :remote))
             (om/query->ast (parser {} [aliased-from] :remote))))
      (is (= (om/query->ast (plain-parser {} [{aliased-from joined-query}] :remote))
             (om/query->ast (parser {} [{aliased-from joined-query}] :remote))))
      (is (= (om/query->ast (plain-parser {} [{(list aliased-from params) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-from params) joined-query}] :remote))))


      ;; With a :< param, it should alias the key and pass on the remaining params.
      (is (= (set/rename-keys (plain-parser {} [aliased-from] nil)
                              {aliased-from aliased-to})
             (parser {} [(list aliased-to {:< aliased-from})] nil)))
      (is (= (set/rename-keys (plain-parser {} [{aliased-from joined-query}] nil)
                              {aliased-from aliased-to})
             (parser {} [{(list aliased-to {:< aliased-from}) joined-query}] nil)))
      (is (= (set/rename-keys (plain-parser {} [{(list aliased-from params) joined-query}] nil)
                              {aliased-from aliased-to})
             (parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] nil)))

      ;; But for remote queries, it should pass the :< along to the remote.
      (is (= (om/query->ast (plain-parser {} [(list aliased-to {:< aliased-from})] :remote))
             (om/query->ast (parser {} [(list aliased-to {:< aliased-from})] :remote))))
      (is (= (om/query->ast (plain-parser {} [{(list aliased-to {:< aliased-from}) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-to {:< aliased-from}) joined-query}] :remote))))
      (is (= (om/query->ast (plain-parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] :remote))
             (om/query->ast (parser {} [{(list aliased-to (assoc params :< aliased-from)) joined-query}] :remote)))))))

(deftest param-indexed-read
  (let [parser (om/parser {:read (param-indexing/read default-db/read)})
        state (atom {:app/current-user [:user/by-id 123]
                     :user/by-id {123 {:user/favorite-color :color/blue
                                       :user/pet {{:pet/name "Milo"} {:pet/name "Milo"
                                                                      :pet/species :pet-species/cat
                                                                      :pet/description "orange tabby"}
                                                  {:pet/name "Otis"} [:pet/by-id 654]}}}
                     :pet/by-id {654 {:pet/name "Otis"
                                      :pet/species :pet-species/dog
                                      :pet/description "pug"}}})]

    (testing "Fetches from a param-indexed key."
      (is (= {:app/current-user {:user/favorite-color :color/blue
                                 :user/pet {:pet/name "Milo"
                                            :pet/species :pet-species/cat}}}
             (parser {:state state} '[{:app/current-user
                                       [:user/favorite-color
                                        {(:user/pet {:pet/name "Milo"})
                                         [:pet/name :pet/species]}]}]))))

    (testing "Fetches from a param-indexed key through an ident."
      (is (= {:app/current-user {:user/favorite-color :color/blue
                                 :user/pet {:pet/name "Otis"
                                            :pet/species :pet-species/dog}}}
             (parser {:state state} '[{:app/current-user
                                       [:user/favorite-color
                                        {(:user/pet {:pet/name "Otis"})
                                         [:pet/name :pet/species]}]}]))))

    (testing "Passes on remote queries."
      (is (= '[{:app/current-user
                [:user/favorite-color
                 {(:user/pet {:pet/name "Milo"})
                  [:pet/name :pet/species]}]}]
             (parser {:state state} '[{:app/current-user
                                       [:user/favorite-color
                                        {(:user/pet {:pet/name "Milo"})
                                         [:pet/name :pet/species]}]}]
                     :some-remote))))))

(deftest query-mapping-read
  (let [inner-read (fn [{:keys [query ast parser] {:keys [key]} :ast :as env} dispatch-key params]
                     ;; A bit of an odd implementation to keep things as simple
                     ;; as possible in the unit test. For most keys, returns a
                     ;; map describing how the inner-read was called.
                     ;; For :user-for-selected-pet, recurses once, to provide
                     ;; the correct structure for the query-mapping-parser to
                     ;; walk to find the pet (which, once again, is actually a
                     ;; map describing how the inner-read was called.
                     {:value (if (= :user-for-selected-pet key)
                               (parser env query)
                               {:key key :params params :query query})
                      :remote true})
        parser (om/parser
                {:read
                 (->> inner-read
                      (query-mapping/read :app/route-params
                                          {:route-params/selected-user
                                           [[:selected-user]
                                            (fn [query {:keys [username]}]
                                              `{(:selected-user {:< :root/user :user/name ~username}) ~query})]
                                           :route-params/selected-pet
                                           [[:user-for-selected-pet :user/pet]
                                            (fn [query {:keys [username pet-name]}]
                                              `{(:user-for-selected-pet {:< :root/user :user/name ~username})
                                                [{(:user/pet {:pet/name ~pet-name})
                                                  ~query}]})]}))})

        state (atom {:app/route-params {:username "nipponfarm" :pet-name "Otis"}})]

    (testing "Maps keys locally according to its config"
      (is (= {:route-params/selected-user {:key :selected-user
                                           :params {:< :root/user :user/name "nipponfarm"}
                                           :query [:user/favorite-color]}
              :route-params/selected-pet {:key :user/pet
                                          :params {:pet/name "Otis"}
                                          :query [:pet/name :pet/species]}}
             (parser {:state state}
                     [{:route-params/selected-user [:user/favorite-color]}
                      {:route-params/selected-pet [:pet/name :pet/species]}]))))

    (testing "Maps keys remotely according to its config"
      ;; (The params can appear in multiple places in the query syntax, so we
      ;; convert the results to AST form where it's unambiguous before comparing.)
      (is (= (om/query->ast '[{(:selected-user {:< :root/user
                                                :user/name "nipponfarm"})
                               [:user/favorite-color]}
                              {(:user-for-selected-pet {:< :root/user
                                                        :user/name "nipponfarm"})
                               [{(:user/pet {:pet/name "Otis"})
                                 [:pet/name :pet/species]}]}])
             (om/query->ast (parser {:state state}
                                    [{:route-params/selected-user [:user/favorite-color]}
                                     {:route-params/selected-pet [:pet/name :pet/species]}]
                                    :remote)))))))

(deftest all-together-now
  (let [parser
        (om/parser
         {:read (->> default-db/read
                     (query-mapping/read
                      :app/route-params
                      {:route-params/selected-user [[:selected-user]
                                                    (fn [query {:keys [username]}]
                                                      `{(:selected-user {:< :root/user :user/name ~username}) ~query})]
                       :route-params/selected-pet [[:user-for-selected-pet :user/pet]
                                                   (fn [query {:keys [username pet-name]}]
                                                     `{(:user-for-selected-pet {:< :root/user :user/name ~username})
                                                       [{(:user/pet {:pet/name ~pet-name})
                                                         ~query}]})]})
                     param-indexing/read
                     aliasing/read)})
        state (atom {:app/current-user [:user/by-id 123]
                     :app/route-params {:username "nipponfarm" :pet-name "Otis"}
                     :root/user {{:user/name "nipponfarm"} [:user/by-id 123]
                                 {:user/name "jburnford"} [:user/by-id 456]}
                     :user/by-id {123 {:user/name "nipponfarm"
                                       :user/favorite-color :color/blue
                                       :user/favorite-number 42
                                       :user/favorite-fellow-user [:user/by-id 456]
                                       :user/pet {{:pet/name "Milo"} [:pet/by-id 321]
                                                  {:pet/name "Otis"} [:pet/by-id 654]}}
                                  456 {:user/name "jburnford"
                                       :user/favorite-color :color/red
                                       :user/favorite-number 7
                                       :user/pet {{:pet/name "Chance"} [:pet/by-id 987]
                                                  {:pet/name "Shadow"} [:pet/by-id 210]
                                                  {:pet/name "Sassy"} [:pet/by-id 543]}}}
                     :pet/by-id {321 {:pet/name "Milo"
                                      :pet/species :pet-species/cat
                                      :pet/description "orange tabby"}
                                 654 {:pet/name "Otis"
                                      :pet/species :pet-species/dog
                                      :pet/description "pug"}
                                 987 {:pet/name "Chance"
                                      :pet/species :pet-species/dog
                                      :pet/description "American bulldog"}
                                 210 {:pet/name "Shadow"
                                      :pet/species :pet-species/dog
                                      :pet/description "golden retriever"}
                                 543 {:pet/name "Sassy"
                                      :pet/species :pet-species/cat
                                      :pet/description "Himalayan"}}})]

    (let [query '[{(:current-user-1 {:< :app/current-user})
                   [:user/favorite-color
                    {(:milo {:< :user/pet :pet/name "Milo"})
                     [:pet/name :pet/species]}
                    {(:otis {:< :user/pet :pet/name "Otis"})
                     [:pet/name :pet/species]}]}
                  {(:current-user-2 {:< :app/current-user})
                   [:user/favorite-number
                    {(:favorite-user-1 {:< :user/favorite-fellow-user})
                     [:user/favorite-color
                      {:user/favorite-fellow-user ...}]}
                    {(:favorite-user-2 {:< :user/favorite-fellow-user})
                     [:user/favorite-number
                      {:user/favorite-fellow-user ...}]}]}
                  {:route-params/selected-user [:user/favorite-color]}
                  {:route-params/selected-pet [:pet/name :pet/species]}]]
      (is (= {:current-user-1 {:user/favorite-color :color/blue
                               :milo {:pet/name "Milo"
                                      :pet/species :pet-species/cat}
                               :otis {:pet/name "Otis"
                                      :pet/species :pet-species/dog}}
              :current-user-2 {:user/favorite-number 42
                               :favorite-user-1 {:user/favorite-color :color/red}
                               :favorite-user-2 {:user/favorite-number 7}}
              :route-params/selected-user {:user/favorite-color :color/blue}
              :route-params/selected-pet {:pet/name "Otis"
                                          :pet/species :pet-species/dog}}
             (parser {:state state} '[{(:current-user-1 {:< :app/current-user})
                                       [:user/favorite-color
                                        {(:milo {:< :user/pet :pet/name "Milo"})
                                         [:pet/name :pet/species]}
                                        {(:otis {:< :user/pet :pet/name "Otis"})
                                         [:pet/name :pet/species]}]}
                                      {(:current-user-2 {:< :app/current-user})
                                       [:user/favorite-number
                                        {(:favorite-user-1 {:< :user/favorite-fellow-user})
                                         [:user/favorite-color
                                          {:user/favorite-fellow-user ...}]}
                                        {(:favorite-user-2 {:< :user/favorite-fellow-user})
                                         [:user/favorite-number
                                          {:user/favorite-fellow-user ...}]}]}
                                      {:route-params/selected-user [:user/favorite-color]}
                                      {:route-params/selected-pet [:pet/name :pet/species]}])))
      (is (= (om/query->ast
              '[{(:current-user-1 {:< :app/current-user})
                 [:user/favorite-color
                  {(:milo {:< :user/pet :pet/name "Milo"})
                   [:pet/name :pet/species]}
                  {(:otis {:< :user/pet :pet/name "Otis"})
                   [:pet/name :pet/species]}]}
                {(:current-user-2 {:< :app/current-user})
                 [:user/favorite-number
                  {(:favorite-user-1 {:< :user/favorite-fellow-user})
                   [:user/favorite-color
                    {:user/favorite-fellow-user ...}]}
                  {(:favorite-user-2 {:< :user/favorite-fellow-user})
                   [:user/favorite-number
                    {:user/favorite-fellow-user ...}]}]}
                {(:selected-user {:< :root/user :user/name "nipponfarm"})
                 [:user/favorite-color]}
                {(:user-for-selected-pet {:< :root/user :user/name "nipponfarm"})
                 [{(:user/pet {:pet/name "Otis"})
                   [:pet/name :pet/species]}]}])
             (om/query->ast
              (parser {:state state} '[{(:current-user-1 {:< :app/current-user})
                                        [:user/favorite-color
                                         {(:milo {:< :user/pet :pet/name "Milo"})
                                          [:pet/name :pet/species]}
                                         {(:otis {:< :user/pet :pet/name "Otis"})
                                          [:pet/name :pet/species]}]}
                                       {(:current-user-2 {:< :app/current-user})
                                        [:user/favorite-number
                                         {(:favorite-user-1 {:< :user/favorite-fellow-user})
                                          [:user/favorite-color
                                           {:user/favorite-fellow-user ...}]}
                                         {(:favorite-user-2 {:< :user/favorite-fellow-user})
                                          [:user/favorite-number
                                           {:user/favorite-fellow-user ...}]}]}
                                       {:route-params/selected-user [:user/favorite-color]}
                                       {:route-params/selected-pet [:pet/name :pet/species]}]
                      :some-remote)))))))


(def my-merge
  (bodhi/merge
   (-> bodhi/basic-merge
       bodhi/normalizing-merge
       param-indexing/merge
       aliasing/merge
       bodhi/key-identifying-merge)))


(deftest test-merge
  (testing "Updates a simple prop on the root"
    (let [state {:app/a-number 111
                 :app/a-string "Hello, there."}
          novelty {:app/a-number 222}
          query [:app/a-number]]
      (is (= {:keys #{:app/a-number}
              :next {:app/a-number 222
                     :app/a-string "Hello, there."}}
             (my-merge {} state novelty query)))))

  (testing "Updates a prop through a join"
    (let [state {:app/current-user {:user/name "nipponfarm"
                                    :user/favorite-color :color/blue
                                    :user/favorite-number 42}}
          novelty {:app/current-user {:user/favorite-number 57}}
          query [{:app/current-user [:user/favorite-number]}]]
      (is (= {:keys #{:user/favorite-number}
              :next {:app/current-user {:user/name "nipponfarm"
                                        :user/favorite-color :color/blue
                                        :user/favorite-number 57}}}
             (my-merge {} state novelty query)))))

  (testing "Normalizes data"
    (let [User (ui
                 static om/Ident
                 (ident [this props] [:user/by-name (:user/name props)])
                 static om/IQuery
                 (query [this] [:user/name :user/favorite-number]))
          Root (ui
                 static om/IQuery
                 (query [this] [{:app/current-user (om/get-query User)}]))
          state {}
          novelty {:app/current-user {:user/name "nipponfarm"
                                      :user/favorite-number 57}}
          query (om/get-query Root)]
      (is (= {:keys #{:user/name :user/favorite-number}
              :next {:app/current-user [:user/by-name "nipponfarm"]
                     :user/by-name {"nipponfarm" {:user/name "nipponfarm"
                                                  :user/favorite-number 57}}}}
             (my-merge {} state novelty query))))

    ;; Bit of an odd test case to demonstrate that a component which doesn't
    ;; implement Ident doesn't break normalization.
    (let [User (ui
                 static om/Ident
                 (ident [this props] [:user/by-name (:user/name props)])
                 static om/IQuery
                 (query [this] [:user/name :user/favorite-number]))
          Page (ui
                 static om/IQuery
                 (query [this] [{:app/current-user (om/get-query User)}]))
          Root (ui
                 static om/IQuery
                 (query [this] [{:page/user (om/get-query Page)}]))
          state {}
          novelty {:page/user {:app/current-user {:user/name "nipponfarm"
                                                  :user/favorite-number 57}}}
          query (om/get-query Root)]
      (is (= {:keys #{:user/name :user/favorite-number}
              :next {:page/user {:app/current-user [:user/by-name "nipponfarm"]}
                     :user/by-name {"nipponfarm" {:user/name "nipponfarm"
                                                  :user/favorite-number 57}}}}
             (my-merge {} state novelty query)))))

  (testing "Updates with an aliased query"
    (let [state {:app/current-user {:user/name "nipponfarm"
                                    :user/favorite-color :color/blue
                                    :user/favorite-number 42}}
          novelty {:the-user {:the-color :color/green}
                   :the-user-again {:the-number 57}}
          query '[{(:the-user {:< :app/current-user}) [(:the-color {:< :user/favorite-color})]}
                  {(:the-user-again {:< :app/current-user}) [(:the-number {:< :user/favorite-number})]}]]
      (is (= {:keys #{:the-color :the-number}
              :next {:app/current-user {:user/name "nipponfarm"
                                        :user/favorite-color :color/green
                                        :user/favorite-number 57}}}
             (my-merge {} state novelty query))))

    (testing "with normalization"
      (let [UserWithColor (ui
                            static om/Ident
                            (ident [this props] [:user/by-name (:user/name props)])
                            static om/IQuery
                            (query [this] '[:user/name (:the-color {:< :user/favorite-color})]))
            UserWithNumber (ui
                             static om/Ident
                             (ident [this props] [:user/by-name (:user/name props)])
                             static om/IQuery
                             (query [this] '[:user/name (:the-number {:< :user/favorite-number})]))
            Root (ui
                   static om/IQuery
                   (query [this] `[{(:the-user {:< :app/current-user}) ~(om/get-query UserWithColor)}
                                   {(:the-user-again {:< :app/current-user}) ~(om/get-query UserWithNumber)}]))
            state {}
            novelty {:the-user {:user/name "nipponfarm"
                                :the-color :color/green}
                     :the-user-again {:user/name "nipponfarm"
                                      :the-number 57}}
            query (om/get-query Root)]
        (is (= {:keys #{:user/name :the-color :the-number}
                :next {:app/current-user [:user/by-name "nipponfarm"]
                       :user/by-name {"nipponfarm" {:user/name "nipponfarm"
                                                    :user/favorite-color :color/green
                                                    :user/favorite-number 57}}}}
               (my-merge {} state novelty query))))))

  (testing "Stores data by params"
    (let [state {:app/current-user {:user/name "nipponfarm"
                                    :user/favorite-color :color/blue
                                    :user/favorite-number 42}}
          novelty {:app/current-user {:milo {:pet/name "Milo"
                                             :pet/species :pet-species/cat}
                                      :otis {:pet/name "Otis"
                                             :pet/species :pet-species/dog}}}
          query '[{:app/current-user [{(:milo {:< :user/pet :pet/name "Milo"})
                                       [:pet/name :pet/species]}
                                      {(:otis {:< :user/pet :pet/name "Otis"})
                                       [:pet/name :pet/species]}]}]]
      (is (= {:keys #{:pet/name :pet/species}
              :next {:app/current-user {:user/name "nipponfarm"
                                        :user/favorite-color :color/blue
                                        :user/favorite-number 42
                                        :user/pet {{:pet/name "Milo"} {:pet/name "Milo"
                                                                       :pet/species :pet-species/cat}
                                                   {:pet/name "Otis"} {:pet/name "Otis"
                                                                       :pet/species :pet-species/dog}}}}}
             (my-merge {} state novelty query))))))
