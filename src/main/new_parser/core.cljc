(ns new-parser.core
  (:require [om.next :as om]
            [om.next.impl.parser :as om-parser]))

(defn aliasing-parser [next-parser]
  (om/parser
   {:read
    (fn [{:keys [target ast query] {:keys [key]} :ast :as env} dispatch-key params]
      (if target
        {target (om-parser/expr->ast (first (next-parser env [(om-parser/ast->expr ast)] target)))}
        {:value
         (let [next-parser-key (:< params key)
               next-params (dissoc params :<)
               join-key-expr (if (empty? next-params)
                               next-parser-key
                               (list next-parser-key next-params))
               next-query [{join-key-expr query}]]
           (get (next-parser env next-query nil) next-parser-key))}))}))


