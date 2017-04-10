(ns bodhi.param-indexing
  (:refer-clojure :exclude [merge])
  (:require [om.util :as om-util]))

(defn read [next-read]
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

(defn merge [next-merge]
  (fn [{:keys [ast path] :as env}]
    (let [{:keys [key params]} ast]
      (if (seq params)
        (next-merge (assoc env
                           :ast (dissoc ast :params)
                           :path (conj path params)))
        (next-merge env)))))
