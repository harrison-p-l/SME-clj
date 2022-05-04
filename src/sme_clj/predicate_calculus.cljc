(ns sme-clj.predicate-calculus
  (:require
   [sme-clj.typedef :as types]
   [clojure.walk    :as walk]))

(defmethod types/initialize-kg :default [_] {})

(defn lookup [kg k & props] (reduce (fn [k prop] (get-in kg [k prop])) k props))

(defmethod types/attribute? :default
  [kg k]
  ((comp (partial = :sme/Attribute) #(lookup kg % :pc/type)) k))

(defmethod types/entity? :default
  [kg k]
  ((comp (partial = :sme/Entity) #(lookup kg % :pc/type)) k))

(defmethod types/expression? :default
  [kg k]
  ((comp (partial = :sme/Expression) #(lookup kg % :pc/type)) k))

(defmethod types/expression-args :default
  [kg k]
  (when (types/expression? kg k)
    (map (comp (partial vector :pc/arg) list) (lookup kg k :pc/args))))

(defmethod types/expression-functor :default
  [kg k]
  (when (types/expression? kg k) (lookup kg k :sme/functor)))

(defmethod types/type-function? :default
  [kg k]
  (= :sme/Function (lookup kg k :pc/type)))

(defmethod types/ordered? :default [kg k] (lookup kg k :sme/ordered?))

(defmethod types/add-entity :default
  [m id & slots]
  (let [e {:pc/id    id
           :pc/type  :sme/Entity
           :pc/slots slots}]
    (assoc m id e)))

(defmethod types/add-predicate :default
  [m [id & {:as p}]]
  (let [p (merge {:pc/id        id
                  :pc/type      :sme/Relation
                  :sme/arity    2
                  :sme/ordered? true}
                 p
                 (when-not (= (:pc/type p) :sme/Relation) {:sme/arity 1}))]
    (assoc m id p)))

(defmethod types/add-concept-graph :default
  [m concept-graph-id & expressions]
  (let [e-map (atom [])]
    (letfn [(add-expr! [[functor & other-args :as args]]
              (let [id (types/combine-ids args)]
                (swap! e-map conj
                  (assoc {:pc/id       id
                          :pc/type     :sme/Expression
                          :sme/functor functor
                          :pc/args     other-args}
                         :sme/concept-graph
                         concept-graph-id))
                id))]
      ;; Doseq is used here instead of passing all expressions to postwalk to
      ;; prevent the
      ;; entire set of expressions for the concept graph being counted as an
      ;; expression.
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      (as-> m m
        (reduce (fn [m
                     {:pc/keys [id]
                      :as      e}]
                  (assoc m id e))
                m
                @e-map)
        (assoc m
               concept-graph-id
               {:pc/id   concept-graph-id
                :pc/type :sme/ConceptGraph
                :pc/spec expressions})))))

(defmethod types/extract-common-role-fillers :default
  [_ & ms]
  (->> ms
       (map (partial map second))
       (apply map vector)))

(defmethod types/expressions :default
  [kg concept-graph-name]
  (->> kg
       vals
       (filter (comp (partial = concept-graph-name) :sme/concept-graph))
       (map :pc/id)))

(defmethod types/direct-children :default
  [kg mh]
  (->> mh
       (map #(->> %
                  (types/expression-args kg)
                  (mapcat second)))
       (apply map vector)
       set))

(defmethod types/same-functor? :default
  [kg ks]
  (->> ks
       (map (partial types/expression-functor kg))
       ((every-pred (partial every? some?) (partial apply =)))))
