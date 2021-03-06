(def +version+ "0.0.0-SNAPSHOT")

(set-env!
 :source-paths #{"src"}
 :dependencies '[[org.clojure/clojure         "1.9.0-alpha14"  :scope "provided"]
                 [org.clojure/clojurescript   "1.9.473"        :scope "provided"]
                 [org.omcljs/om               "1.0.0-alpha47"  :scope "provided"]
                 [crisptrutski/boot-cljs-test "0.3.0"          :scope "test"]
                 [adzerk/bootlaces            "0.1.13"         :scope "test"]
                 [org.clojure/test.check      "0.9.0"          :scope "test"]
                 [com.gfredericks/test.chuck  "0.2.7"          :scope "test"]
                 [cljsjs/react                "15.3.1-0"       :scope "test"]]
 :exclusions '[org.clojure/clojure org.clojure/clojurescript])

(require
 '[adzerk.bootlaces :refer [bootlaces! push-release]]
 '[crisptrutski.boot-cljs-test :as bt-cljs])

(bootlaces! +version+ :dont-modify-paths? true)

(task-options!
  pom {:project 'bodhi
       :version +version+
       :description "FIXME"
       :url "FIXME"
       :scm {:url "FIXME"}
       :license {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask build-jar []
  (set-env! :resource-paths #{"src"})
  (adzerk.bootlaces/build-jar))

(deftask release-clojars! []
  (comp
    (build-jar)
    (push-release)))

(deftask deps [])

(ns-unmap 'boot.user 'test)

(deftask test
  [e exit?     bool  "Enable flag."]
  (set-env! :source-paths #(conj % "test"))
  (let [exit? (cond-> exit?
                (nil? exit?) not)]
    (comp
     (bt-cljs/test-cljs
      :js-env :node
      :namespaces #{'bodhi.tests}
      :cljs-opts {:parallel-build true}
      :exit? exit?))))

(deftask auto-test []
  (comp
    (watch)
    (test)))
