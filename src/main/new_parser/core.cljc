(ns new-parser.core
  (:require [om.next :as om]
            [om.next.impl.parser :as om-parser]
            [om.util :as om-util]))

(defn basic-parser []
  (om/parser
   {:read
    (fn [{:keys [state parser query] {:keys [key]} :ast :as env} _ _]
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
               value))))})}))

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
               next-query [{join-key-expr query}]]
           (get (next-parser env next-query) next-parser-key))}))}))


