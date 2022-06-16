(ns sme-clj.util
  "Utility/helper functions"
  (:require
   [clojure.walk          :as walk]
   [datascript.core       :as d]
   [mops.datascript-mops  :as dm]
   [ont-app.datascript-graph.core :as dsg]
   [tuh8888.clojure-utils :refer [cond-pred->]]))



(defn mhs->labels
  [conn mhs]
  (->> mhs
       (map (partial map (comp :mops/id (partial d/pull @conn [:mops/id]))))
       set))

(defn mhs->ids
  [conn mhs]
  (->> mhs
       (map (partial map
                     (comp (partial d/entid @conn) (partial vector :mops/id))))
       set))

(defn inferences->labels
  [conn inferences]
  (walk/postwalk (fn [x]
                   (cond-pred-> x
                                integer?
                                (->> (d/pull @conn [:mops/id])
                                     :mops/id)))
                 inferences))

(defn standardize-db
  [db]
  (-> db
      (d/datoms :eavt)
      (->> (map (partial drop-last 2))
           (map vec))
      set))
