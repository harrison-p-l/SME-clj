(ns sme-clj.test-utils
  (:require
   [clojure.walk          :as walk]
   [datascript.core       :as d]
   [ont-app.datascript-graph.core :as dsg]
   [tuh8888.clojure-utils :refer [cond-pred->]]))

(defn mhs->labels
  [g mhs]
  (->> mhs
       (map (partial map (comp ::dsg/id (partial d/pull (:db g) [::dsg/id]))))
       set))

(defn mhs->ids
  [g mhs]
  (->> mhs
       (map (partial map
                     (comp (partial d/entid (:db g))
                           (partial vector ::dsg/id))))
       set))

(defn inferences->labels
  [g inferences]
  (walk/postwalk (fn [x]
                   (cond-pred-> x
                                integer?
                                (->> (d/pull (:db g) [::dsg/id])
                                     ::dsg/id)))
                 inferences))

(defn standardize-db
  [db]
  (-> db
      (d/datoms :eavt)
      (->> (map (partial drop-last 2))
           (map vec))
      set))

(defn labels->ids
  [g labels]
  (map (comp (partial d/entid (:db g)) (partial vector ::dsg/id)) labels))


(defn ids->labels
  [g ids]
  (map (comp ::dsg/id (partial d/pull (:db g) [::dsg/id])) ids))

(defn gmaps-mhs->ids
  [g gmaps]
  (map #(update % :mhs
          (partial map
                   (partial mapv
                            (comp (partial d/entid (:db g))
                                  (partial vector ::dsg/id)))))
       gmaps))
