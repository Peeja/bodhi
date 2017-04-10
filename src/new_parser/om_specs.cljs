(ns new-parser.om-specs
  (:require [clojure.spec :as s]
            [clojure.test.check.generators :as gen]))

;; Adapted from https://gist.github.com/swannodette/842326cbb6ab35e0d1aa51fb41c5697e

(s/def ::ident
  (s/with-gen
    (s/and vector? (s/cat :ident keyword? :value any?))
    #(gen/tuple gen/keyword-ns gen/any)))

(s/def ::join-key (s/or :prop keyword? :ident ::ident))

(s/def ::join-target (s/or :no-params ::join-key
                           :with-params (s/cat :join-key ::join-key
                                               :params map?)))

(s/def ::joined-query
  (s/or
   :recursion (s/or
               :depth (s/and integer? pos?)
               :unbounded (s/with-gen
                            #(= '... %)
                            #(gen/return '...)))
   :query ::query))

(s/def ::join (s/with-gen
                (s/and (s/map-of ::join-target ::joined-query) #(= (count %) 1))
                (fn []
                  (gen/map (s/gen ::join-target)
                           (gen/scale #(quot % 2) (s/gen ::joined-query))
                           {:num-elements 1}))))

;; FIXME: Unions may be meant to be able to join to recursive queries, in
;; which case this should use ::joined-query instead of ::query. However, the Om
;; parser fails for such queries, so if that should be correct, there's a bug
;; which prevents it from working.
;; https://github.com/omcljs/om/issues/855
(s/def ::union (s/with-gen
                 (s/and
                  (s/map-of ::join-target (s/and (s/map-of keyword? ::query) #(> (count %) 1)))
                  #(= (count %) 1))
                 (fn []
                   (gen/map (s/gen ::join-target)
                            (gen/map gen/keyword-ns
                                     (gen/scale #(quot % 2) (s/gen ::query))
                                     {:min-elements 2 :max-elements 3})
                            {:num-elements 1}))))

(s/def ::params map?)

(s/def ::param-expr
  (s/cat :query-expr ::query-expr
         :params ::params))

(s/def ::mutation-expr
  (s/or :no-params (s/cat :mutate-key symbol?)
        :with-params (s/cat :mutate-key symbol?
                            :params map?)))

(s/def ::query-expr
  (s/or :prop keyword?
        :ident ::ident
        :mutation-expr ::mutation-expr
        :join ::join
        :union ::union
        :param-expr ::param-expr))

(s/def ::query
  (s/with-gen
    (s/and vector?
           (s/* ::query-expr))
    #(gen/vector (s/gen ::query-expr))))
