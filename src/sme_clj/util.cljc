(ns sme-clj.util
  "Utility/helper functions"
  (:require
   [datascript.core :as d]
   [mops.core       :as mops]))

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
  (->> [:mops/id concept-graph]
       (d/q '{:find  [[?id ...]]
              :in    [$ ?c]
              :where [[?x :mops/id ?id] [?x :sme/concept-graph ?c]]}
            @conn)
       (map (juxt identity (partial mops/get-mop conn)))
       (into {})))
