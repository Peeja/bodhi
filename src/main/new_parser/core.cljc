(ns new-parser.core
  (:require [om.next :as om]
            [om.util :as om-util]))

(defn aliasing-parser [next-parser]
  (om/parser
   {:read
    (fn [{:keys [query] {:keys [key]} :ast :as env} dispatch-key params]
      (let [next-parser-key (:< params key)
            next-params (dissoc params :<)
            join-key-expr (if (empty? next-params)
                            next-parser-key
                            (list next-parser-key next-params))
            next-query [{join-key-expr query}]]
        {:value (get (next-parser env next-query nil) next-parser-key)}))}))


