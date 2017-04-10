(ns bodhi.default-db
  (:require [om.util :as om-util]))

(defn read [{:keys [parser target state query] {:keys [key]} :ast :as env} _ _]
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
