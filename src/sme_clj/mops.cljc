(ns sme-clj.mops
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require
   [clojure.walk    :as walk]
   #?(:clj [mops.records]
      :cljs [mops.records :refer [MopMap]])
   [mops.core       :as mops]
   [sme-clj.typedef :as types]
   [clojure.set     :as set])
  (:import #?(:clj [mops.records MopMap])))

(defmethod types/attribute? MopMap [kg k] (mops/abstr? kg k :sme/Attribute))

(defmethod types/entity? MopMap [kg k] (mops/abstr? kg k :sme/Entity))

(defmethod types/expression? MopMap [kg k] (mops/abstr? kg k :sme/Expression))

(defmethod types/expression-args MopMap
  [kg k]
  (when (types/expression? kg k)
    (let [mop (mops/get-mop kg k)]
      (->> mop
           mops/roles
           (remove #{:sme/concept-graph :sme/functor :mops/parents})
           (map (juxt identity (partial mops/filler-value mop)))))))

(defmethod types/expression-functor MopMap
  [kg k]
  (let [mop (mops/get-mop kg k)]
    (first (mops/filler-value mop :sme/functor))))

(defmethod types/type-function? MopMap [kg k] (mops/abstr? kg k :sme/Function))

(defmethod types/ordered? MopMap
  [kg k]
  (when (mops/get-mop kg k) (mops/inherit-filler kg k :sme/ordered?)))

(defmethod types/add-entity MopMap
  [m id parent slots & args]
  (let [mop (mops/add-slot (mops/->mop id
                                       {}
                                       (-> args
                                           types/args->slots
                                           (merge slots)))
                           :mops/parents
                           parent)]
    (mops/add-mop m mop)))

(defmethod types/add-concept-graph MopMap
  [m concept-graph-id & expressions]
  (let [e-map (atom m)]
    (letfn
      [(add-expr! [& args]
         (let [[[functor & function-args :as all-e]] args
               id                                    (types/combine-ids all-e)]
           (swap! e-map mops/add-mop
             (-> id
                 (mops/->mop {} (types/args->slots function-args))
                 (mops/add-slot :mops/parents :sme/Expression)
                 (mops/add-slot :sme/functor functor)
                 (mops/add-slot :sme/concept-graph concept-graph-id)))
           id))]
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      (mops/add-mop @e-map
                    (-> concept-graph-id
                        (mops/->mop {} {})
                        (mops/add-slot :mops/parents :sme/ConceptGraph))))))

(defmethod types/initialize-kg MopMap
  [m]
  (reduce (partial apply types/add-entity)
          m
          [[:sme/Expression :thing nil]
           [:sme/Entity :thing nil]
           [:sme/Functor :sme/Expression nil]
           [:sme/Relation :sme/Functor {:sme/ordered? true}]
           [:sme/Attribute :sme/Functor nil]
           [:sme/Function :sme/Functor nil]]))

(defmethod types/extract-common-role-fillers MopMap
  [_ & ms]
  (let [ms           (map (partial into {}) ms)
        common-roles (->> ms
                          (map keys)
                          (map set)
                          (apply set/intersection))]
    (map (apply juxt ms) common-roles)))

(defmethod types/expressions MopMap
  [kg concept-graph-name]
  (-> kg
      :mops
      (map (mops/mop-ids kg))
      (->> (filter (comp #{concept-graph-name}
                         first
                         #(mops/filler-value % :sme/concept-graph)))
           (map (comp :mops/id meta)))))

(defmethod types/direct-children MopMap
  [kg mh]
  (->> mh
       (map (partial types/expression-args kg))
       (apply concat)
       (group-by first)
       (mapcat (fn [[_ args]]
                 (let [[base target] (map second args)]
                   (for [b base
                         t target]
                     [b t]))))
       set))
