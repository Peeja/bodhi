(ns bodhi.core
  (:refer-clojure :exclude [merge])
  (:require [om.next :as om]))

(defn key-identifying-merge [next-merge]
  (fn [{:keys [ast] :as env}]
    (let [{:keys [key type]} ast]
      (cond-> (next-merge env)
        (= :prop type) (update :keys conj key)))))

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
