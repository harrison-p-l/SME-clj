(ns sme-clj.datascript-mops
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  (:require
   [clojure.walk    :as walk]
   [mops.core       :as mops]
   [sme-clj.typedef :as types]
   [clojure.set     :as set]
   [datascript.core :as d])
  (:import #?(:clj [clojure.lang Atom])))

(defmethod types/attribute? Atom [kg k] (mops/abstr? kg k :sme/Attribute))

(defmethod types/entity? Atom [kg k] (mops/abstr? kg k :sme/Entity))

(defmethod types/expression? Atom [kg k] (mops/abstr? kg k :sme/Expression))

(defmethod types/expression-args Atom
  [conn k]
  (when (types/expression? conn k)
    (let [mop (mops/get-mop conn k)]
      (->> mop
           mops/roles
           (remove #{:sme/concept-graph :sme/functor :mops/parents})
           (map (juxt identity
                      (comp (partial map
                                     (comp :mops/id
                                           (partial d/pull @conn [:mops/id])
                                           :db/id))
                            (partial mops/filler-value mop))))))))

(defmethod types/expression-functor Atom
  [conn k]
  (let [mop (mops/get-mop conn k)]
    (when-let [id (:db/id (first (mops/filler-value mop :sme/functor)))]
      (:mops/id (d/pull @conn [:mops/id] id)))))

(defmethod types/type-function? Atom [kg k] (mops/abstr? kg k :sme/Function))

(defmethod types/ordered? Atom
  [kg k]
  (when (mops/get-mop kg k) (mops/inherit-filler kg k :sme/ordered?)))

(defmethod types/add-entity Atom
  [conn id parent slots & args]
  (let [mop (mops/add-slot (mops/->mop id
                                       {}
                                       (-> args
                                           types/args->slots
                                           (merge slots)))
                           :mops/parents
                           parent)]
    (mops/add-mop conn mop)))

(defmethod types/add-concept-graph Atom
  [conn concept-graph-id & expressions]
  (letfn
    [(add-expr! [& args]
       (let [[[functor & function-args :as all-e]] args
             id                                    (types/combine-ids all-e)]
         (mops/add-mop conn
                       (-> id
                           (mops/->mop {} (types/args->slots function-args))
                           (mops/add-slot :mops/parents :sme/Expression)
                           (mops/add-slot :sme/functor functor)
                           (mops/add-slot :sme/concept-graph concept-graph-id)))
         id))]
    (doseq [expression expressions]
      (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
    (mops/add-mop conn
                  (-> concept-graph-id
                      (mops/->mop {} {})
                      (mops/add-slot :mops/parents :sme/ConceptGraph)))))

(defmethod types/initialize-kg Atom
  [conn]
  (reduce (partial apply types/add-entity)
          conn
          [[:sme/Expression :thing nil]
           [:sme/Entity :thing nil]
           [:sme/Functor :sme/Expression nil]
           [:sme/Relation :sme/Functor {:sme/ordered? true}]
           [:sme/Attribute :sme/Functor nil]
           [:sme/Function :sme/Functor nil]]))

(defmethod types/extract-common-role-fillers Atom
  [_ & ms]
  (let [ms           (map (partial into {}) ms)
        common-roles (->> ms
                          (map keys)
                          (map set)
                          (apply set/intersection))]
    (map (apply juxt ms) common-roles)))

(defmethod types/expressions Atom
  [conn concept-graph-name]
  (->> (d/q '{:find  [[(pull ?e [:mops/id]) ...]]
              :in    [$ ?concept-graph]
              :where [[?e :sme/concept-graph ?concept-graph]]}
            @conn
            [:mops/id concept-graph-name])
       (map :mops/id)))

(defmethod types/direct-children Atom
  [conn [b t]]
  (->>
   (d/q '{:find  [(pull ?b-filler [:mops/id]) (pull ?t-filler [:mops/id])]
          :in    [$ ?b ?t ?arg?]
          :where [[?b ?role ?b-filler] [?t ?role ?t-filler] [(?arg? ?role)]]}
        @conn
        [:mops/id b]
        [:mops/id t]
        (fn [role] (not (#{"mops" "sme"} (namespace role)))))
   (map (partial map :mops/id))))
