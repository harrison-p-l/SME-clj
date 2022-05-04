(ns sme-clj.typedef-test
  (:require
   [clojure.test         :refer [deftest is testing]]
   [mops.records         :as mr]
   [mops.datascript-mops :as ds-mops]
   [sme-clj.datascript-mops]
   [sme-clj.mops]
   [sme-clj.predicate-calculus]
   [sme-clj.simple-water-heat :refer [kg mops-kg]]
   [sme-clj.typedef      :as sut]
   [taoensso.timbre      :as log]
   [sme-clj.util         :as util]))

(deftest make-concept-graph-test
  (log/with-level :warn
   (testing "Vector definition -> Predicate calculus"
    (is
     (=
      {:diameter-Vial {:pc/id             :diameter-Vial
                       :pc/type           :sme/Expression
                       :sme/functor       :diameter
                       :pc/args           [:Vial]
                       :sme/concept-graph :simple-water-flow}
       :flow-Beaker-Vial-Water-Pipe {:pc/id :flow-Beaker-Vial-Water-Pipe
                                     :pc/type :sme/Expression
                                     :sme/functor :flow
                                     :pc/args [:Beaker :Vial :Water :Pipe]
                                     :sme/concept-graph :simple-water-flow}
       :simple-water-flow
       {:pc/id   :simple-water-flow
        :pc/type :sme/ConceptGraph
        :pc/spec [[:cause
                   [:greater [:pressure :Beaker] [:pressure :Vial]]
                   [:flow :Beaker :Vial :Water :Pipe]]
                  [:greater [:diameter :Beaker] [:diameter :Vial]]
                  [:clear :Beaker]
                  [:flat-top :Water]
                  [:liquid :Water]]}
       :diameter-Beaker {:pc/id             :diameter-Beaker
                         :pc/type           :sme/Expression
                         :sme/functor       :diameter
                         :pc/args           [:Beaker]
                         :sme/concept-graph :simple-water-flow}
       :clear-Beaker {:pc/id             :clear-Beaker
                      :pc/type           :sme/Expression
                      :sme/functor       :clear
                      :pc/args           [:Beaker]
                      :sme/concept-graph :simple-water-flow}
       :pressure-Vial {:pc/id             :pressure-Vial
                       :pc/type           :sme/Expression
                       :sme/functor       :pressure
                       :pc/args           [:Vial]
                       :sme/concept-graph :simple-water-flow}
       :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
       {:pc/id
        :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
        :pc/type :sme/Expression
        :sme/functor :cause
        :pc/args [:greater-pressure-Beaker-pressure-Vial
                  :flow-Beaker-Vial-Water-Pipe]
        :sme/concept-graph :simple-water-flow}
       :greater-pressure-Beaker-pressure-Vial
       {:pc/id             :greater-pressure-Beaker-pressure-Vial
        :pc/type           :sme/Expression
        :sme/functor       :greater
        :pc/args           [:pressure-Beaker :pressure-Vial]
        :sme/concept-graph :simple-water-flow}
       :liquid-Water {:pc/id             :liquid-Water
                      :pc/type           :sme/Expression
                      :sme/functor       :liquid
                      :pc/args           [:Water]
                      :sme/concept-graph :simple-water-flow}
       :greater-diameter-Beaker-diameter-Vial
       {:pc/id             :greater-diameter-Beaker-diameter-Vial
        :pc/type           :sme/Expression
        :sme/functor       :greater
        :pc/args           [:diameter-Beaker :diameter-Vial]
        :sme/concept-graph :simple-water-flow}
       :pressure-Beaker {:pc/id             :pressure-Beaker
                         :pc/type           :sme/Expression
                         :sme/functor       :pressure
                         :pc/args           [:Beaker]
                         :sme/concept-graph :simple-water-flow}
       :flat-top-Water {:pc/id             :flat-top-Water
                        :pc/type           :sme/Expression
                        :sme/functor       :flat-top
                        :pc/args           [:Water]
                        :sme/concept-graph :simple-water-flow}}
      (sut/add-concept-graph {} :simple-water-flow
        [:cause
         [:greater [:pressure :Beaker] [:pressure :Vial]]
         [:flow :Beaker :Vial :Water :Pipe]]
        [:greater [:diameter :Beaker] [:diameter :Vial]]
        [:clear :Beaker]
        [:flat-top :Water]
        [:liquid :Water]))))
   (testing "Vector definition -> MopMap"
    (is
     (=
      {:ids {}
       :mops
       {:diameter-Vial {:e1                {:Vial {}}
                        :mops/parents      {:sme/Expression {}}
                        :sme/functor       {:diameter {}}
                        :sme/concept-graph {:simple-water-flow {}}}
        :flow-Beaker-Vial-Water-Pipe {:e1                {:Beaker {}}
                                      :sme/concept-graph {:simple-water-flow {}}
                                      :e2                {:Vial {}}
                                      :e3                {:Water {}}
                                      :e4                {:Pipe {}}
                                      :mops/parents      {:sme/Expression {}}
                                      :sme/functor       {:flow {}}}
        :simple-water-flow {:mops/parents {:sme/ConceptGraph {}}}
        :diameter-Beaker {:e1                {:Beaker {}}
                          :mops/parents      {:sme/Expression {}}
                          :sme/functor       {:diameter {}}
                          :sme/concept-graph {:simple-water-flow {}}}
        :clear-Beaker {:e1                {:Beaker {}}
                       :mops/parents      {:sme/Expression {}}
                       :sme/functor       {:clear {}}
                       :sme/concept-graph {:simple-water-flow {}}}
        :pressure-Vial {:e1                {:Vial {}}
                        :mops/parents      {:sme/Expression {}}
                        :sme/functor       {:pressure {}}
                        :sme/concept-graph {:simple-water-flow {}}}
        :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
        {:e1                {:greater-pressure-Beaker-pressure-Vial {}}
         :e2                {:flow-Beaker-Vial-Water-Pipe {}}
         :mops/parents      {:sme/Expression {}}
         :sme/functor       {:cause {}}
         :sme/concept-graph {:simple-water-flow {}}}
        :greater-pressure-Beaker-pressure-Vial
        {:e1                {:pressure-Beaker {}}
         :e2                {:pressure-Vial {}}
         :mops/parents      {:sme/Expression {}}
         :sme/functor       {:greater {}}
         :sme/concept-graph {:simple-water-flow {}}}
        :liquid-Water {:e1                {:Water {}}
                       :mops/parents      {:sme/Expression {}}
                       :sme/functor       {:liquid {}}
                       :sme/concept-graph {:simple-water-flow {}}}
        :greater-diameter-Beaker-diameter-Vial
        {:e1                {:diameter-Beaker {}}
         :e2                {:diameter-Vial {}}
         :mops/parents      {:sme/Expression {}}
         :sme/functor       {:greater {}}
         :sme/concept-graph {:simple-water-flow {}}}
        :pressure-Beaker {:e1                {:Beaker {}}
                          :mops/parents      {:sme/Expression {}}
                          :sme/functor       {:pressure {}}
                          :sme/concept-graph {:simple-water-flow {}}}
        :flat-top-Water {:e1                {:Water {}}
                         :mops/parents      {:sme/Expression {}}
                         :sme/functor       {:flat-top {}}
                         :sme/concept-graph {:simple-water-flow {}}}}}
      (-> (mr/make-mop-map)
          (sut/add-concept-graph :simple-water-flow
            [:cause
             [:greater [:pressure :Beaker] [:pressure :Vial]]
             [:flow :Beaker :Vial :Water :Pipe]]
            [:greater [:diameter :Beaker] [:diameter :Vial]]
            [:clear :Beaker]
            [:flat-top :Water]
            [:liquid :Water])
          mr/remove-mopiness)))
    (is
     (=
      {:ids  {}
       :mops {:simple-heat-flow {:mops/parents {:sme/ConceptGraph {}}}
              :liquid-Coffee {:e1                {:Coffee {}}
                              :mops/parents      {:sme/Expression {}}
                              :sme/functor       {:liquid {}}
                              :sme/concept-graph {:simple-heat-flow {}}}
              :greater-temperature-Coffee-temperature-Icecube
              {:e1                {:temperature-Coffee {}}
               :e2                {:temperature-Icecube {}}
               :mops/parents      {:sme/Expression {}}
               :sme/functor       {:greater {}}
               :sme/concept-graph {:simple-heat-flow {}}}
              :flat-top-Coffee {:e1                {:Coffee {}}
                                :mops/parents      {:sme/Expression {}}
                                :sme/functor       {:flat-top {}}
                                :sme/concept-graph {:simple-heat-flow {}}}
              :temperature-Coffee {:e1                {:Coffee {}}
                                   :mops/parents      {:sme/Expression {}}
                                   :sme/functor       {:temperature {}}
                                   :sme/concept-graph {:simple-heat-flow {}}}
              :temperature-Icecube {:e1                {:Icecube {}}
                                    :mops/parents      {:sme/Expression {}}
                                    :sme/functor       {:temperature {}}
                                    :sme/concept-graph {:simple-heat-flow {}}}
              :flow-Coffee-Icecube-Heat-Bar {:e1 {:Coffee {}}
                                             :sme/concept-graph
                                             {:simple-heat-flow {}}
                                             :e2 {:Icecube {}}
                                             :e3 {:Heat {}}
                                             :e4 {:Bar {}}
                                             :mops/parents {:sme/Expression {}}
                                             :sme/functor {:flow {}}}}}
      (-> (mr/make-mop-map)
          (sut/add-concept-graph :simple-heat-flow
            [:flow :Coffee :Icecube :Heat :Bar]
            [:greater
             [:temperature :Coffee]
             [:temperature :Icecube]]
            [:flat-top :Coffee]
            [:liquid :Coffee])
          mr/remove-mopiness))))
   (testing "Vector definition -> Datascript Mops"
    (is
     (=
      {:diameter-Vial {:sme/concept-graph [:simple-water-flow]
                       :e1                [:Vial]
                       :sme/functor       [:diameter]
                       :mops/parents      [:sme/Expression]}
       :flow-Beaker-Vial-Water-Pipe {:e1                [:Beaker]
                                     :sme/concept-graph [:simple-water-flow]
                                     :e2                [:Vial]
                                     :e3                [:Water]
                                     :e4                [:Pipe]
                                     :mops/parents      [:sme/Expression]
                                     :sme/functor       [:flow]}
       :diameter-Beaker {:sme/concept-graph [:simple-water-flow]
                         :e1                [:Beaker]
                         :sme/functor       [:diameter]
                         :mops/parents      [:sme/Expression]}
       :clear-Beaker {:sme/concept-graph [:simple-water-flow]
                      :e1                [:Beaker]
                      :sme/functor       [:clear]
                      :mops/parents      [:sme/Expression]}
       :pressure-Vial {:sme/concept-graph [:simple-water-flow]
                       :e1                [:Vial]
                       :sme/functor       [:pressure]
                       :mops/parents      [:sme/Expression]}
       :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
       {:e1                [:greater-pressure-Beaker-pressure-Vial]
        :sme/concept-graph [:simple-water-flow]
        :e2                [:flow-Beaker-Vial-Water-Pipe]
        :mops/parents      [:sme/Expression]
        :sme/functor       [:cause]}
       :greater-pressure-Beaker-pressure-Vial {:e1 [:pressure-Beaker]
                                               :sme/concept-graph
                                               [:simple-water-flow]
                                               :e2 [:pressure-Vial]
                                               :mops/parents [:sme/Expression]
                                               :sme/functor [:greater]}
       :liquid-Water {:sme/concept-graph [:simple-water-flow]
                      :e1                [:Water]
                      :sme/functor       [:liquid]
                      :mops/parents      [:sme/Expression]}
       :greater-diameter-Beaker-diameter-Vial {:e1 [:diameter-Beaker]
                                               :sme/concept-graph
                                               [:simple-water-flow]
                                               :e2 [:diameter-Vial]
                                               :mops/parents [:sme/Expression]
                                               :sme/functor [:greater]}
       :pressure-Beaker {:sme/concept-graph [:simple-water-flow]
                         :e1                [:Beaker]
                         :sme/functor       [:pressure]
                         :mops/parents      [:sme/Expression]}
       :flat-top-Water {:sme/concept-graph [:simple-water-flow]
                        :e1                [:Water]
                        :sme/functor       [:flat-top]
                        :mops/parents      [:sme/Expression]}}
      (-> (ds-mops/make-mop-map)
          (sut/add-concept-graph :simple-water-flow
            [:cause
             [:greater [:pressure :Beaker] [:pressure :Vial]]
             [:flow :Beaker :Vial :Water :Pipe]]
            [:greater [:diameter :Beaker] [:diameter :Vial]]
            [:clear :Beaker]
            [:flat-top :Water]
            [:liquid :Water])
          (util/standardize-mops :simple-water-flow))))
    (is
     (=
      {:flat-top-Coffee                                {:sme/concept-graph
                                                        [:simple-heat-flow]
                                                        :e1 [:Coffee]
                                                        :sme/functor [:flat-top]
                                                        :mops/parents
                                                        [:sme/Expression]}
       :greater-temperature-Coffee-temperature-Icecube {:e1
                                                        [:temperature-Coffee]
                                                        :sme/concept-graph
                                                        [:simple-heat-flow]
                                                        :e2
                                                        [:temperature-Icecube]
                                                        :mops/parents
                                                        [:sme/Expression]
                                                        :sme/functor [:greater]}
       :liquid-Coffee                                  {:sme/concept-graph
                                                        [:simple-heat-flow]
                                                        :e1 [:Coffee]
                                                        :sme/functor [:liquid]
                                                        :mops/parents
                                                        [:sme/Expression]}
       :temperature-Coffee                             {:sme/concept-graph
                                                        [:simple-heat-flow]
                                                        :e1 [:Coffee]
                                                        :sme/functor
                                                        [:temperature]
                                                        :mops/parents
                                                        [:sme/Expression]}
       :temperature-Icecube                            {:sme/concept-graph
                                                        [:simple-heat-flow]
                                                        :e1 [:Icecube]
                                                        :sme/functor
                                                        [:temperature]
                                                        :mops/parents
                                                        [:sme/Expression]}
       :flow-Coffee-Icecube-Heat-Bar                   {:e1 [:Coffee]
                                                        :sme/concept-graph
                                                        [:simple-heat-flow]
                                                        :e2 [:Icecube]
                                                        :e3 [:Heat]
                                                        :e4 [:Bar]
                                                        :mops/parents
                                                        [:sme/Expression]
                                                        :sme/functor [:flow]}}
      (-> (ds-mops/make-mop-map)
          (sut/add-concept-graph :simple-heat-flow
            [:flow :Coffee :Icecube :Heat :Bar]
            [:greater
             [:temperature :Coffee]
             [:temperature :Icecube]]
            [:flat-top :Coffee]
            [:liquid :Coffee])
          (util/standardize-mops :simple-heat-flow)))))))

(deftest add-entity-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is (= {:a {:pc/id    :a
                :pc/type  :sme/Entity
                :pc/slots nil}}
           (sut/add-entity {} :a))))))

(deftest add-predicate-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is
     (=
      {:greater {:pc/id        :greater
                 :pc/type      :sme/Relation
                 :sme/arity    3
                 :sme/ordered? true}}
      (sut/add-predicate {} [:greater :pc/type :sme/Relation :sme/arity 3]))))))

(def expected-concept-graph-expressions
  #{:diameter-Vial :flow-Beaker-Vial-Water-Pipe :diameter-Beaker :clear-Beaker
    :liquid-Coffee :pressure-Vial
    :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
    :greater-temperature-Coffee-temperature-Icecube :flat-top-Coffee
    :greater-pressure-Beaker-pressure-Vial :temperature-Coffee :liquid-Water
    :temperature-Icecube :greater-diameter-Beaker-diameter-Vial :pressure-Beaker
    :flat-top-Water :flow-Coffee-Icecube-Heat-Bar})

(deftest expressions-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is (= expected-concept-graph-expressions
           (into #{}
                 (lazy-cat (sut/expressions kg :simple-water-flow)
                           (sut/expressions kg :simple-heat-flow))))))
   (testing "Mops"
    (is (= expected-concept-graph-expressions
           (into #{}
                 (lazy-cat (sut/expressions mops-kg :simple-heat-flow)
                           (sut/expressions mops-kg :simple-water-flow))))))))

(deftest find-children-test
  (log/with-level :warn
   (let [expected-found-children1 #{}
         expected-found-children2 #{[:Beaker :Coffee]
                                    [:Water :Heat]
                                    [:Pipe :Bar]
                                    [:Vial :Icecube]}]
     (testing "Predicate calculus"
      (is (= expected-found-children1
             (set (sut/direct-children kg [:Beaker :Coffee]))))
      (is (= expected-found-children2
             (set (sut/direct-children kg
                                       [:flow-Beaker-Vial-Water-Pipe
                                        :flow-Coffee-Icecube-Heat-Bar])))))
     (testing "Mops"
      (is (= expected-found-children1
             (set (sut/direct-children mops-kg [:Beaker :Coffee]))))
      (is (= expected-found-children2
             (set (sut/direct-children mops-kg
                                       [:flow-Beaker-Vial-Water-Pipe
                                        :flow-Coffee-Icecube-Heat-Bar]))))))))
