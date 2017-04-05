(ns new-parser.tests
  (:require #?(:cljs [cljsjs.react])
            [clojure.test :refer [deftest testing is]]
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
    (is (thrown-with-msg? #?(:clj AssertionError :cljs js/Error) #":outer-parser"
                          ((new-parser/basic-parser) {} [:some-key]))))

  (let [parser (new-parser/composed-parser (new-parser/basic-parser))
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
    (is (thrown-with-msg? #?(:clj AssertionError :cljs js/Error) #":outer-parser"
                          ((new-parser/param-indexed-parser (om/parser {}))
                           {} [:some-key]))))

  (let [parser ((comp new-parser/composed-parser
                      new-parser/param-indexed-parser
                      new-parser/basic-parser))
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

(deftest query-mapping-parser
  (testing "Requires an outer-parser."
    (is (thrown-with-msg? #?(:clj AssertionError :cljs js/Error) #":outer-parser"
                          ((new-parser/param-indexed-parser (om/parser {}))
                           {} [:some-key]))))

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
        inner-parser (om/parser {:read inner-read})
        parser (->> inner-parser
                    (new-parser/query-mapping-parser :app/route-params
                                                     {:route-params/selected-user
                                                      [[:selected-user]
                                                       (fn [query {:keys [username]}]
                                                         `{(:selected-user {:< :root/user :user/name ~username}) ~query})]
                                                      :route-params/selected-pet
                                                      [[:user-for-selected-pet :user/pet]
                                                       (fn [query {:keys [username pet-name]}]
                                                         `{(:user-for-selected-pet {:< :root/user :user/name ~username})
                                                           [{(:user/pet {:pet/name ~pet-name})
                                                             ~query}]})]})
                    new-parser/composed-parser)
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
  (let [parser ((comp new-parser/composed-parser
                      new-parser/aliasing-parser
                      new-parser/param-indexed-parser
                      (partial new-parser/query-mapping-parser
                               :app/route-params
                               {:route-params/selected-user [[:selected-user]
                                                             (fn [query {:keys [username]}]
                                                               `{(:selected-user {:< :root/user :user/name ~username}) ~query})]
                                :route-params/selected-pet [[:user-for-selected-pet :user/pet]
                                                            (fn [query {:keys [username pet-name]}]
                                                              `{(:user-for-selected-pet {:< :root/user :user/name ~username})
                                                                [{(:user/pet {:pet/name ~pet-name})
                                                                  ~query}]})]})
                      new-parser/basic-parser))
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

(defn aliasing-merge [next-merge]
  (fn [{:keys [ast novelty] :as env}]
    (if-not (contains? (:params ast) :<)
      (next-merge env)
      (let [aliased-from (get-in ast [:params :<])
            next-ast (-> ast
                         (update :params dissoc :<)
                         (assoc :key aliased-from))]
        (next-merge (assoc env
                           :ast next-ast
                           :novelty {aliased-from (get novelty (:key ast))}))))))

(defn basic-merge [{:keys [merge state path novelty ast]}]
  (let [{:keys [key]} ast]
    (case (:type ast)
      :prop {:keys #{key}
             :next (assoc-in state (conj path key) (get novelty key))}
      :join (merge state (conj path key) (get novelty key) ast))))

(defn my-merge* [state path novelty ast]
  (reduce (fn [ret ast]
            (-> ((aliasing-merge basic-merge)
                 {:merge my-merge*
                  :state (:next ret)
                  :path path
                  :novelty novelty
                  :ast ast})
                (update :keys into (:keys ret))))
          {:keys #{}
           :next state}
          (:children ast)))

(defn my-merge [reconciler state novelty query]
  (let [ast (om/query->ast query)]
    (my-merge* state [] novelty ast)))

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

  (testing "Updates with an aliased query"
    (let [state {:app/current-user {:user/name "nipponfarm"
                                    :user/favorite-color :color/blue
                                    :user/favorite-number 42}}
          novelty {:the-user {:the-color :color/green}
                   :the-user-again {:the-number 57}}
          query '[{(:the-user {:< :app/current-user}) [(:the-color {:< :user/favorite-color})]}
                  {(:the-user-again {:< :app/current-user}) [(:the-number {:< :user/favorite-number})]}]]
      (is (= {:keys #{:user/favorite-color :user/favorite-number}
              :next {:app/current-user {:user/name "nipponfarm"
                                        :user/favorite-color :color/green
                                        :user/favorite-number 57}}}
             (my-merge {} state novelty query))))))
