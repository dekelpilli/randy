(ns build
  "
  clojure -T:build deploy

  Run tests:
  clojure -X:test
  "
  (:require
    [build-edn.core :as build-edn]
    [clojure.tools.build.api :as b]))

(def ^:private config
  {:lib             'com.github.dekelpilli/randy
   :version         (format "0.0.%s" (b/git-count-revs nil))
   :github-actions? true})

(defn jar [opts]
  (build-edn/jar (merge config opts)))

(defn install [opts]
  (build-edn/install (merge config opts)))

(defn deploy [opts]
  (build-edn/deploy (merge config opts)))
