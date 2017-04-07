(ns new-parser.core
  (:refer-clojure :exclude [merge])
  (:require [om.next :as om]
            [om.next.impl.parser :as om-parser]
            [om.util :as om-util]))

(defn- assert-outer-parser [outer-parser]
  (assert (fn? outer-parser) "No :outer-parser found in parser env. Don't forget to wrap your parser with composed-parser."))

(defn composed-parser [next-parser]
  (fn self
    ([env query] (self env query nil))
    ([env query target]
     (next-parser (assoc env :outer-parser self) query target))))

(defn basic-parser []
  (om/parser
   {:read
    (fn [{:keys [outer-parser target state query] {:keys [key]} :ast :as env} _ _]
      (assert-outer-parser outer-parser)
      (if target
        {target true}
        {:value
         (let [refs @state
               node (:node env refs)]
           (cond
             (om-util/unique-ident? key)
             (let [ident-key (first key)]
               (get (outer-parser (assoc env :node refs) [{ident-key query}]) ident-key))

             (om-util/ident? key)
             (outer-parser (assoc env :node (get-in refs key)) query)

             :else
             (let [value (get node key)
                   value (if (om-util/ident? value)
                           (get-in refs value)
                           value)
                   query (if (= '[*] query)
                           (vec (keys value))
                           query)]
               (if (and query value)
                 (outer-parser (assoc env :node value) query)
                 value))))}))}))

(defn aliasing-parser [next-parser]
  (om/parser
   {:read
    (fn [{:keys [target ast query] {:keys [key]} :ast :as env} _ params]
      (if target
        {target (om-parser/expr->ast (first (next-parser env [(om-parser/ast->expr ast)] target)))}
        {:value
         (let [next-parser-key (:< params key)
               next-params (dissoc params :<)
               join-key-expr (if (empty? next-params)
                               next-parser-key
                               (list next-parser-key next-params))
               next-query (cond-> join-key-expr
                            query (hash-map query)
                            true vector)]
           (get (next-parser env next-query) next-parser-key))}))}))

(defn param-indexed-parser [next-parser]
  (om/parser
   {:read
    (fn [{:keys [outer-parser target ast state query] {:keys [key]} :ast :as env} _ params]
      (assert-outer-parser outer-parser)
      (if target
        {target (om-parser/expr->ast (first (next-parser env [(om-parser/ast->expr ast)] target)))}
        {:value
         (let [refs @state
               node (:node env refs)]
           (if (seq params)
             (let [next-node (get-in node [key params])
                   next-node (if (om-util/ident? next-node)
                               (get-in refs next-node)
                               next-node)]
               (outer-parser (assoc env :node next-node) query))
             (let [next-query (cond-> key
                                query (hash-map query)
                                true vector)]
               (get (next-parser env next-query) key))))}))}))

(defn query-mapping-parser [data-key config next-parser]
  (om/parser
   {:read
    (fn [{:keys [outer-parser target ast state query] {:keys [key]} :ast :as env} _ params]
      (assert-outer-parser outer-parser)
      (let [refs @state]
        (if target
          {target
           (if-let [[path next-expr-fn] (get config key)]
             (let [next-expr (next-expr-fn query (get refs data-key))]
               (om-parser/expr->ast (first (outer-parser env [next-expr] target))))
             (om-parser/expr->ast (first (next-parser env [(om-parser/ast->expr ast)] target))))}
          {:value
           (if-let [[path next-expr-fn] (get config key)]
             (let [next-expr (next-expr-fn query (get refs data-key))]
               (get-in (outer-parser env [next-expr]) path))
             (get (next-parser env [(om-parser/ast->expr ast)]) key))})))}))




(defn key-identifying-merge [next-merge]
  (fn [{:keys [ast] :as env}]
    (let [{:keys [key type]} ast]
      (cond-> (next-merge env)
        (= :prop type) (update :keys conj key)))))

(defn aliasing-merge [next-merge]
  (fn [{:keys [ast path] :as env}]
    (let [{:keys [key params]} ast]
      (if-let [aliased-from (:< params)]
        (next-merge (assoc env
                           :ast (update ast :params dissoc :<)
                           :path (-> path pop (conj aliased-from))))
        (next-merge env)))))

(defn param-indexed-merge [next-merge]
  (fn [{:keys [ast path] :as env}]
    (let [{:keys [key params]} ast]
      (if (seq params)
        (next-merge (assoc env
                           :ast (dissoc ast :params)
                           :path (conj path params)))
        (next-merge env)))))

(defn normalizing-merge [next-merge]
  (fn [{:keys [path novelty ast] :as env}]
    (let [{:keys [key component]} ast]
      (if #?(:clj (satisfies? om/Ident component)
             :cljs (implements? om/Ident component))
        (let [ident (om/ident component (get novelty key))]
          (next-merge (-> env
                          (update :state assoc-in path ident)
                          (assoc :path ident))))
        (next-merge env)))))

(defn basic-merge [{:keys [merge state path novelty ast]}]
  (let [{:keys [key type]} ast]
    (case type
      :prop {:keys #{}
             :next (assoc-in state path (get novelty key))}
      :join (merge state path (get novelty key) ast))))

(defn merge [merge-novelty]
  (fn [reconciler state novelty query]
    (let [inner-merge
          (fn self [state path novelty ast]
            (reduce (fn [ret ast]
                      (-> (merge-novelty
                           {:merge self
                            :state (:next ret)
                            :path (conj path (:key ast))
                            :novelty novelty
                            :ast ast})
                          (update :keys into (:keys ret))))
                    {:keys #{}
                     :next state}
                    (:children ast)))]
      (inner-merge state [] novelty (om/query->ast query)))))
