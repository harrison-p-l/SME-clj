(ns sme-clj.simple-water-heat
  (:require
   [mops.records         :as mr]
   [mops.core            :as mops]
   [sme-clj.typedef      :as types]
   [sme-clj.mops]
   [sme-clj.predicate-calculus]
   [sme-clj.datascript-mops]
   [mops.datascript-mops :as ds-mops]))

(def kg
  (as-> (types/initialize-kg {}) m
    (reduce
     (partial apply types/add-entity)
     m
     [[:Coffee] [:Icecube] [:Bar] [:Heat] [:Water] [:Beaker] [:Vial] [:Pipe]])
    (reduce (fn [m args] (types/add-predicate m args))
            m
            [[:flow :pc/type :sme/Relation :sme/arity 4]
             [:greater :pc/type :sme/Relation :sme/arity 2]
             [:cause :pc/type :sme/Relation :sme/arity 2]
             [:temperature :pc/type :sme/Function]
             [:flat-top :pc/type :sme/Function]
             [:pressure :pc/type :sme/Function]
             [:diameter :pc/type :sme/Function]
             [:liquid :pc/type :sme/Attribute]
             [:clear :pc/type :sme/Attribute]])
    (-> m
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
          [:liquid :Coffee]))))

(def mops-kg
  (-> (reduce (partial apply types/add-entity)
              (types/initialize-kg (mr/make-mop-map))
              [[:cause :sme/Relation nil :sme/Expression :sme/Expression]
               [:greater :sme/Relation nil :sme/Expression :sme/Expression]
               [:flow
                :sme/Relation
                nil
                #_{:sme/ordered? {false {}}}
                :sme/Entity
                :sme/Entity
                :sme/Entity
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

(def ds-kg
  (-> (reduce (partial apply types/add-entity)
              (types/initialize-kg (ds-mops/make-mop-map))
              [[:cause :sme/Relation nil :sme/Expression :sme/Expression]
               [:greater :sme/Relation nil :sme/Expression :sme/Expression]
               [:flow
                :sme/Relation
                nil
                #_{:sme/ordered? {false {}}}
                :sme/Entity
                :sme/Entity
                :sme/Entity
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
        [:liquid :Coffee])))
