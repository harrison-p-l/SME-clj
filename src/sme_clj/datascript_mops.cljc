(ns sme-clj.datascript-mops
  "Data type definitions and protocols for SME, as well as facilities to create
   instances of types and manipulate them."
  #?@(:clj [(:require
             [clojure.set          :as set]
             [clojure.walk         :as walk]
             [mops.datascript-mops :as dm]
             [ont-app.datascript-graph.core :as dsg]
             [sme-clj.typedef      :as types])
            (:import (ont_app.datascript_graph.core DatascriptGraph))]
      :cljs [(:require
              [clojure.set           :as set]
              [clojure.walk          :as walk]
              [mops.datascript-mops  :as dm]
              [ont-app.datascript-graph.core :as dsg]
              [sme-clj.typedef       :as types]
              [tuh8888.clojure-utils :as util])
             (:import (ont_app.datascript_graph.core DatascriptGraph))]))

;;!zprint {:format :skip}
(def ds-rules
  '[[(p? ?e ?p)
     (ancestor? ?e [::dsg/id :mops/parents] ?p)]

    [(functor-p? ?e ?p)
     (edge ?e [::dsg/id :sme/functor] ?f)
     (p? ?f ?p)]

    [(same-functor? ?b-filler ?t-filler)
     (edge ?b-filler [::dsg/id :sme/functor] ?filler-functor)
     (edge ?t-filler [::dsg/id :sme/functor] ?filler-functor)]

    #_[(ordered-functor? ?e)
       [?e :sme/functor ?functor]
       (or-join
         [?functor]
         [?functor :sme/ordered? true]
         (and
           (not [?functor :sme/ordered? false])
           (parent-filler? ?functor :mops/parents ?p :sme/ordered? true)))]])

(defn structural-role?
  [role]
  (not (#{"mops" "sme" "mops.role"} (namespace role))))

(defmethod types/attribute? DatascriptGraph
  [g k]
  (dm/abstr? g k [::dsg/id :sme/Attribute]))

(defmethod types/entity? DatascriptGraph
  [g k]
  (dm/abstr? g k [::dsg/id :sme/Entity]))

(defmethod types/expression? DatascriptGraph
  [g k]
  (dm/abstr? g k [::dsg/id :sme/Expression]))

(defmethod types/expression-args DatascriptGraph
  [g k]
  (-> g
      (dm/query-with-rules '{:find  [?role ?filler]
                             :in    [?e ?arg?]
                             :where [(p? ?e [::dsg/id :sme/Expression])
                                     (edge ?e ?role ?filler)
                                     [?role ::dsg/id ?role-id]
                                     [(?arg? ?role-id)]]}
                           ds-rules
                           k
                           structural-role?)
      (->> (group-by first))
      (#?(:clj update-vals
          :cljs util/cljs-update-vals)
       (partial map second))))

(defmethod types/expression-functor DatascriptGraph
  [g k]
  (dm/query g
            '{:find  [?f .]
              :in    [?e]
              :where [(edge ?e [::dsg/id :sme/functor] ?f)]}
            k))

(defmethod types/type-function? DatascriptGraph
  [g k]
  (dm/abstr? g k [::dsg/id :sme/Function]))

(defmethod types/ordered? DatascriptGraph
  [g k]
  (dm/query
   g
   '{:find  [[?f .]]
     :in    [?k]
     :where [(or-bind
              [?f]
              (edge ?k :sme/ordered? ?f)
              (and
               (ancestor? ?k ?abstr-role ?a)
               (abstr? ?abstr-role)
               (edge ?a :sme/ordered? ?f)))]}
   k))

(defmethod types/add-entity DatascriptGraph
  [g id parent slots & args]
  (-> args
      types/args->slots
      (merge slots)
      (->> (apply concat)
           (into [id :mops/parents parent])
           (dm/add-mop g)))
  #_(let [mop (mops/add-slot (mops/->mop id
                                         {}
                                         (-> args
                                             types/args->slots
                                             (merge slots)))
                             :mops/parents
                             parent)]
      (mops/add-mop g mop)))

(defmethod types/add-concept-graph DatascriptGraph
  [g concept-graph-id & expressions]
  (let [ag (atom g)]
    (letfn
      [(add-expr! [& args]
         (let [[[functor & function-args :as all-e]] args
               id                                    (types/combine-ids all-e)]
           (swap! ag dm/add-mop
             (-> function-args
                 types/args->slots
                 (#?(:clj update-vals
                     :cljs util/cljs-update-vals)
                  (fn [filler] {filler {:sme/concept-graph concept-graph-id}}))
                 (->> (apply concat)
                      (into [id :mops/parents :sme/Expression :sme/functor
                             functor #_#_:sme/concept-graph concept-graph-id])))
             #_(-> id
                   (mops/->mop {})
                   (mops/add-slot :mops/parents :sme/Expression)
                   (mops/add-slot :sme/functor functor)
                   (mops/add-slot :sme/concept-graph concept-graph-id)))
           id))]
      (doseq [expression expressions]
        (walk/postwalk #(cond->> % (coll? %) add-expr!) expression))
      (dm/add-mop @ag
                  [concept-graph-id :mops/parents :sme/ConceptGraph]
                  #_(-> concept-graph-id
                        (mops/->mop {} {})
                        (mops/add-slot :mops/parents :sme/ConceptGraph))))))

(defmethod types/initialize-kg DatascriptGraph
  [g]
  (reduce (partial apply types/add-entity)
          (dm/def-abstr-role g :mops/parents)
          [[:sme/Expression :thing nil]
           [:sme/Entity :thing nil]
           [:sme/Functor :sme/Expression nil]
           [:sme/Relation :sme/Functor {:sme/ordered? true}]
           [:sme/Attribute :sme/Functor nil]
           [:sme/Function :sme/Functor nil]]))

(defmethod types/extract-common-role-fillers DatascriptGraph
  [_ & ms]
  (let [ms           (map (partial into {}) ms)
        common-roles (->> ms
                          (map keys)
                          (map set)
                          (apply set/intersection))]
    (map (apply juxt ms) common-roles)))

(defmethod types/expressions DatascriptGraph
  [g concept-graph-name]
  (dm/query g
            '{:find  [[?s ...]]
              :in    [?concept-graph]
              :where [(edge ?s _ _ ?e) [?e :sme/concept-graph ?concept-graph]]}
            [::dsg/id concept-graph-name]))

(defmethod types/direct-children DatascriptGraph
  [g [b t]]
  (dm/query g
            '{:find  [?b-filler ?t-filler]
              :in    [?b ?t ?arg?]
              :where [(edge ?b ?role ?b-filler)
                      (edge ?t ?role ?t-filler)
                      [?role ::dsg/id ?role-id]
                      [(?arg? ?role-id)]]}
            b
            t
            (fn [role] (not (#{"mops" "sme"} (namespace role))))))

(defmethod types/same-functor? DatascriptGraph
  [g [b t]]
  (dm/query g
            '{:find  [?b ?t]
              :in    [?b ?t]
              :where [(edge ?b :sme/functor ?f) (edge ?t :sme/functor ?f)]}
            b
            t))

(defn standardize-mops
  [g]
  (letfn [(uv [p f m]
            (-> m
                (->> (group-by p))
                (#?(:clj update-vals
                    :cljs util/cljs-update-vals)
                 f)))]
    (-> g
        (dm/query '{:find  [?s-id ?p-id ?o-id ?prop ?attr-id]
                    :keys  [s p o prop attr]
                    :where [(edge ?s ?p ?o ?e)
                            [?s ::dsg/id ?s-id]
                            [?p ::dsg/id ?p-id]
                            [?o ::dsg/id ?o-id]
                            [?e ?prop ?attr]
                            [?attr ::dsg/id ?attr-id]
                            [(not= ?prop ::dsg/id)]
                            [(not= ?prop ::dm/from)]]})
        ((partial
          uv
          :s
          (partial
           uv
           :p
           (partial uv
                    :o
                    (comp (partial into {})
                          (partial remove (comp #{::dm/to ::dm/label} first))
                          (partial map (juxt :prop :attr))))))))))
