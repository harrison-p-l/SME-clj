(ns water-heat
  (:require
   [mops.records    :as mr]
   [mops.core       :as mops]
   [sme-clj.core    :as sme]
   [sme-clj.typedef :as types]
   [sme-clj.ruledef :as rules]))

(def kg
  (-> (reduce (partial apply types/add-entity)
              (types/initialize-kg (mr/make-mop-map))
              [[:cause :sme/Relation nil :sme/Expression :sme/Expression]
               [:greater :sme/Relation nil :sme/Expression :sme/Expression]
               [:flow :sme/Relation nil :sme/Entity :sme/Entity :sme/Entity
                :sme/Entity]
               [:pressure :sme/Function nil :sme/Entity]
               [:diameter :sme/Function nil :sme/Entity]
               [:clear :sme/Attribute nil :sme/Entity]
               [:temperature :sme/Function nil :sme/Entity]
               [:flat-top :sme/Function nil :sme/Entity]
               [:liquid :sme/Attribute nil :sme/Entity]
               [:Coffee :sme/Entity nil]
               [:Water :sme/Entity nil]
               [:Heat :sme/Entity nil]
               [:Pipe :sme/Entity nil]
               [:Vial :sme/Entity nil]
               [:Icecube :sme/Entity nil]
               [:Bar :sme/Entity nil]
               [:Beaker :sme/Entity nil]])
      (types/add-concept-graph :simple-water-flow
        [:cause
         [:greater [:pressure :Beaker] [:pressure :Vial]]
         [:flow :Beaker :Vial :Water :Pipe]]
        [:greater [:diameter :Beaker] [:diameter :Vial]]
        [:clear :Beaker]
        [:flat-top :Water]
        [:liquid :Water])
      (types/add-concept-graph :simple-heat-flow
        [:flow :Coffee :Icecube :Heat :Bar]
        [:greater
         [:temperature :Coffee]
         [:temperature :Icecube]]
        [:flat-top :Coffee]
        [:liquid :Coffee])
      mops/infer-hierarchy))

(->> rules/analogy
     (sme/match kg
       :simple-water-flow :simple-heat-flow)
     (sme/perform-inference kg :simple-water-flow)
     (sort-by :score)
     (reverse)
     (map (juxt :score :transferred)))
