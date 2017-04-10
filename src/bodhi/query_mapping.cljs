(ns bodhi.query-mapping
  (:require [om.next.impl.parser :as om-parser]))

(defn read [data-key config next-read]
  (fn [{:keys [parser target ast state query] {:keys [key]} :ast :as env} k params]
    (let [refs @state]
      (if-let [[path next-expr-fn] (get config key)]
        (let [next-expr (next-expr-fn query (get refs data-key))
              mapped-result (parser env [next-expr] target)]
          {target (om-parser/expr->ast (first mapped-result))
           :value (get-in mapped-result path)})
        (next-read env k params)))))
