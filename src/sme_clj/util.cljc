(ns sme-clj.util
  "Utility/helper functions"
  (:require
   [datascript.core :as d]
   [mops.core       :as mops]
   [mops.datascript-mops :as ds-mops]))

(defn vals-as-keys [k m] (zipmap (map k m) m))

(defmacro cond-pred->
  "Like cond-> but also threads initial expr through tests."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g     (gensym)
        steps (map (fn [[test step]]
                     `(if (-> ~g
                              ~test)
                        (-> ~g
                            ~step)
                        ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defn standardize-mops
  [conn concept-graph]
  (letfn [(realize-mop [mop]
            (-> mop
                ds-mops/realize-mop
                (assoc :mops/parents
                       (->> :mops/parents
                            (mops/filler-value mop)
                            (map (comp :mops/id
                                       (partial d/pull @conn '[:mops/id])
                                       :db/id))))))]
    (->> (d/q '{:find  [[?id ...]]
                :in    [$ ?c]
                :where [[?x :mops/id ?id] [?x :sme/concept-graph ?c]]}
              @conn
              [:mops/id concept-graph])
         (map (juxt identity (comp realize-mop (partial mops/get-mop conn))))
         (into {}))))
