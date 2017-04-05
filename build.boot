(def +version+ "0.0.0-SNAPSHOT")

(set-env!
 :source-paths    #{"src/main"}
 :resource-paths  #{"resources"}
 :dependencies '[[org.clojure/clojure         "1.9.0-alpha14"  :scope "provided"]
                 [org.clojure/clojurescript   "1.9.473"        :scope "provided"]
                 [org.omcljs/om               "1.0.0-alpha47"  :scope "provided"]
                 [adzerk/boot-test            "1.2.0"          :scope "test"]
                 [crisptrutski/boot-cljs-test "0.3.0"          :scope "test"]
                 [adzerk/bootlaces            "0.1.13"         :scope "test"]
                 [org.clojure/test.check      "0.9.0"          :scope "test"]
                 [com.gfredericks/test.chuck  "0.2.7"          :scope "test"]
                 [cljsjs/react                "15.3.1-0"       :scope "test"]]
 :exclusions '[org.clojure/clojure org.clojure/clojurescript])

(require
 '[adzerk.boot-test :as bt-clj]
 '[adzerk.bootlaces :refer [bootlaces! push-release]]
 '[crisptrutski.boot-cljs-test :as bt-cljs])

(bootlaces! +version+ :dont-modify-paths? true)

(task-options!
  pom {:project 'new-parser
       :version +version+
       :description "FIXME"
       :url "FIXME"
       :scm {:url "FIXME"}
       ;; FIXME Add :license
       })

(deftask build-jar []
  (set-env! :resource-paths #{"src/main"})
  (adzerk.bootlaces/build-jar))

(deftask release-clojars! []
  (comp
    (build-jar)
    (push-release)))

(deftask deps [])

(deftask testing []
  (set-env! :source-paths #(conj % "src/test"))
  identity)

(deftask test-clj []
  (comp
    (testing)
    (bt-clj/test)))

(deftask test-cljs
  [e exit?     bool  "Enable flag."]
  (let [exit? (cond-> exit?
                (nil? exit?) not)]
    (comp
      (testing)
      (bt-cljs/test-cljs
        :js-env :node
        :namespaces #{'new-parser.tests}
        :cljs-opts {:parallel-build true}
        :exit? exit?))))

(ns-unmap 'boot.user 'test)

(deftask test []
  (comp
    (test-clj)
    (test-cljs)))

(deftask auto-test []
  (comp
    (watch)
    (notify)
    (test-cljs)))
