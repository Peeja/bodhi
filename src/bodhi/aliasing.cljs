(ns bodhi.aliasing
  (:refer-clojure :exclude [merge])
  (:require [om.util :as om-util]))

(defn read [next-read]
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

(defn merge [next-merge]
  (fn [{:keys [ast path] :as env}]
    (let [{:keys [key params]} ast]
      (if-let [aliased-from (:< params)]
        (next-merge (assoc env
                           :ast (update ast :params dissoc :<)
                           :path (-> path pop (conj aliased-from))))
        (next-merge env)))))
