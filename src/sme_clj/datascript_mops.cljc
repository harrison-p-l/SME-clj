(ns sme-clj.datascript-mops
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require
   [clojure.walk    :as walk]
   [mops.datascript-mops]
   [mops.core       :as mops]
   [sme-clj.typedef :as types]
   [clojure.set     :as set])
  (:import #?(:clj [clojure.lang Atom])))

(defmethod types/attribute? Atom [kg k] (mops/abstr? kg k ::types/Attribute))

(defmethod types/entity? Atom [kg k] (mops/abstr? kg k ::types/Entity))

(defmethod types/expression? Atom [kg k] (mops/abstr? kg k ::types/Expression))

(defmethod types/expression-args Atom
  [kg k]
  (when (types/expression? kg k)
    (let [mop (mops/get-mop kg k)]
      (->> mop
           mops/roles
           (remove #{:concept-graph :functor :parents})
           (map (juxt identity (partial mops/filler-value mop)))))))

(defmethod types/expression-functor Atom
  [kg k]
  (let [mop (mops/get-mop kg k)]
    (first (mops/filler-value mop :functor))))

(defmethod types/type-function? Atom [kg k] (mops/abstr? kg k ::types/Function))

(defmethod types/ordered? Atom
  [kg k]
  (when (mops/get-mop kg k) (mops/inherit-filler kg k :ordered?)))

(defmethod types/add-entity Atom
  [m id parent slots & args]
  (let [mop (mops/add-slot (mops/->mop id
                                       {}
                                       (-> args
                                           types/args->slots
                                           (merge slots)))
                           :parents
                           parent)]
    (mops/add-mop m mop)))

(defmethod types/add-concept-graph Atom
  [conn concept-graph-id & expressions]
  (letfn
    [(add-expr! [& args]
       (let [[[functor & function-args :as all-e]] args
             id                                    (types/combine-ids all-e)]
         (mops/add-mop conn
                       (-> id
                           (mops/->mop {} (types/args->slots function-args))
                           (mops/add-slot :parents ::types/Expression)
                           (mops/add-slot :functor functor)
                           (mops/add-slot :concept-graph concept-graph-id)))
         id))]
    (doseq [expression expressions]
      (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
    (mops/add-mop conn
                  (-> concept-graph-id
                      (mops/->mop {} {})
                      (mops/add-slot :parents ::types/ConceptGraph)))))

(defmethod types/initialize-kg Atom
  [m]
  (reduce (partial apply types/add-entity)
          m
          [[::types/Expression :thing nil] [::types/Entity :thing nil]
           [::types/Functor ::types/Expression nil]
           [::types/Relation ::types/Functor {:ordered? true}]
           [::types/Attribute ::types/Functor nil]
           [::types/Function ::types/Functor nil]]))

(defmethod types/extract-common-role-fillers Atom
  [_ & ms]
  (let [ms           (map (partial into {}) ms)
        common-roles (->> ms
                          (map keys)
                          (map set)
                          (apply set/intersection))]
    (map (apply juxt ms) common-roles)))

(defmethod types/expressions Atom
  [kg concept-graph-name]
  (-> kg
      :mops
      (map (mops/mop-ids kg))
      (->> (filter (comp #{concept-graph-name}
                         first
                         #(mops/filler-value % :concept-graph)))
           (map (comp :id meta)))))

(defmethod types/direct-children Atom
  [kg mh]
  (->> mh
       (map #(->> %
                  (types/expression-args kg)
                  #_(mapcat second)))
       (apply concat)
       (group-by first)
       (mapcat (fn [[group args]]
                 (let [[base target] (map second args)]
                   (for [b base
                         t target]
                     [b t]))))
       #_(apply map vector)
       set))
