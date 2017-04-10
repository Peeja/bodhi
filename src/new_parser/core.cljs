(ns new-parser.core
  (:refer-clojure :exclude [merge])
  (:require [om.next :as om]
            [om.next.impl.parser :as om-parser]
            [om.util :as om-util]))

(defn basic-read [{:keys [parser target state query] {:keys [key]} :ast :as env} _ _]
  (if target
    {target true}
    {:value
     (let [refs @state
           node (:node env refs)]
       (cond
         (om-util/unique-ident? key)
         (let [ident-key (first key)]
           (get (parser (assoc env :node refs) [{ident-key query}]) ident-key))

         (om-util/ident? key)
         (parser (assoc env :node (get-in refs key)) query)

         :else
         (let [value (get node key)
               value (if (om-util/ident? value)
                       (get-in refs value)
                       value)
               query (if (= '[*] query)
                       (vec (keys value))
                       query)]
           (if (and query value)
             (parser (assoc env :node value) query)
             value))))}))

(defn aliasing-read [next-read]
  (fn [{:keys [target ast query] {:keys [key]} :ast :as env} k params]
    (if-let [aliased-from (and (not target) (:< params))]
      (let [next-ast (-> ast
                         (assoc :key aliased-from)
                         (assoc :dispatch-key (if (om-util/ident? aliased-from)
                                                (first aliased-from)
                                                aliased-from))
                         (update :params #(not-empty (dissoc % :<))))]
        (next-read (assoc env :ast next-ast)
                   (:dispatch-key next-ast)
                   (:params next-ast)))
      (next-read env k params))))

(defn param-indexed-read [next-read]
  (fn [{:keys [parser target ast state query] {:keys [key]} :ast :as env} k params]
    (let [refs @state
          node (:node env refs)]
      (if (and (nil? target) (seq params))
        (let [next-node (get-in node [key params])
              next-node (if (om-util/ident? next-node)
                          (get-in refs next-node)
                          next-node)]
          {:value (parser (assoc env :node next-node) query)})
        (next-read env k params)))))

(defn query-mapping-read [data-key config next-read]
  (fn [{:keys [parser target ast state query] {:keys [key]} :ast :as env} k params]
    (let [refs @state]
      (if-let [[path next-expr-fn] (get config key)]
        (let [next-expr (next-expr-fn query (get refs data-key))
              mapped-result (parser env [next-expr] target)]
          {target (om-parser/expr->ast (first mapped-result))
           :value (get-in mapped-result path)})
        (next-read env k params)))))




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
      (if (implements? om/Ident component)
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
