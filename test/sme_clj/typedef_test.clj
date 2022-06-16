(ns sme-clj.typedef-test
  (:require
   [clojure.test            :refer [deftest is testing]]
   [mops.datascript-mops    :as dm]
   [sme-clj.datascript-mops :as sme-dm]
   [sme-clj.predicate-calculus]
   [sme-clj.simple-water-heat :refer [kg ds-kg]]
   [sme-clj.typedef         :as sut]
   [taoensso.timbre         :as log]
   [sme-clj.util            :as util]))

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
      {:diameter-Vial {:e1           {:Vial {:sme/concept-graph
                                             :simple-water-flow}}
                       :mops/parents {:sme/Expression {}}
                       :sme/functor  {:diameter {}}}
       :flow-Beaker-Vial-Water-Pipe
       {:e1           {:Beaker {:sme/concept-graph :simple-water-flow}}
        :e2           {:Vial {:sme/concept-graph :simple-water-flow}}
        :e3           {:Water {:sme/concept-graph :simple-water-flow}}
        :e4           {:Pipe {:sme/concept-graph :simple-water-flow}}
        :mops/parents {:sme/Expression {}}
        :sme/functor  {:flow {}}}
       :simple-water-flow {:mops/parents {:sme/ConceptGraph {}}}
       :diameter-Beaker {:e1           {:Beaker {:sme/concept-graph
                                                 :simple-water-flow}}
                         :mops/parents {:sme/Expression {}}
                         :sme/functor  {:diameter {}}}
       :clear-Beaker {:e1           {:Beaker {:sme/concept-graph
                                              :simple-water-flow}}
                      :mops/parents {:sme/Expression {}}
                      :sme/functor  {:clear {}}}
       :pressure-Vial {:e1           {:Vial {:sme/concept-graph
                                             :simple-water-flow}}
                       :mops/parents {:sme/Expression {}}
                       :sme/functor  {:pressure {}}}
       :cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
       {:e1           {:greater-pressure-Beaker-pressure-Vial
                       {:sme/concept-graph :simple-water-flow}}
        :e2           {:flow-Beaker-Vial-Water-Pipe {:sme/concept-graph
                                                     :simple-water-flow}}
        :mops/parents {:sme/Expression {}}
        :sme/functor  {:cause {}}}
       :greater-pressure-Beaker-pressure-Vial
       {:e1           {:pressure-Beaker {:sme/concept-graph :simple-water-flow}}
        :e2           {:pressure-Vial {:sme/concept-graph :simple-water-flow}}
        :mops/parents {:sme/Expression {}}
        :sme/functor  {:greater {}}}
       :liquid-Water {:e1           {:Water {:sme/concept-graph
                                             :simple-water-flow}}
                      :mops/parents {:sme/Expression {}}
                      :sme/functor  {:liquid {}}}
       :greater-diameter-Beaker-diameter-Vial
       {:e1           {:diameter-Beaker {:sme/concept-graph :simple-water-flow}}
        :e2           {:diameter-Vial {:sme/concept-graph :simple-water-flow}}
        :mops/parents {:sme/Expression {}}
        :sme/functor  {:greater {}}}
       :pressure-Beaker {:e1           {:Beaker {:sme/concept-graph
                                                 :simple-water-flow}}
                         :mops/parents {:sme/Expression {}}
                         :sme/functor  {:pressure {}}}
       :flat-top-Water {:e1           {:Water {:sme/concept-graph
                                               :simple-water-flow}}
                        :mops/parents {:sme/Expression {}}
                        :sme/functor  {:flat-top {}}}}
      (-> {}
          (dm/make-mop-graph)
          (sut/add-concept-graph :simple-water-flow
            [:cause
             [:greater [:pressure :Beaker] [:pressure :Vial]]
             [:flow :Beaker :Vial :Water :Pipe]]
            [:greater [:diameter :Beaker] [:diameter :Vial]]
            [:clear :Beaker]
            [:flat-top :Water]
            [:liquid :Water])
          sme-dm/standardize-mops)))
    (is
     (=
      {:simple-heat-flow                               {:mops/parents
                                                        {:sme/ConceptGraph {}}}
       :liquid-Coffee                                  {:e1
                                                        {:Coffee
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :mops/parents
                                                        {:sme/Expression {}}
                                                        :sme/functor {:liquid
                                                                      {}}}
       :greater-temperature-Coffee-temperature-Icecube {:e1
                                                        {:temperature-Coffee
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :e2
                                                        {:temperature-Icecube
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :mops/parents
                                                        {:sme/Expression {}}
                                                        :sme/functor {:greater
                                                                      {}}}
       :flat-top-Coffee                                {:e1
                                                        {:Coffee
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :mops/parents
                                                        {:sme/Expression {}}
                                                        :sme/functor {:flat-top
                                                                      {}}}
       :temperature-Coffee                             {:e1
                                                        {:Coffee
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :mops/parents
                                                        {:sme/Expression {}}
                                                        :sme/functor
                                                        {:temperature {}}}
       :temperature-Icecube                            {:e1
                                                        {:Icecube
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :mops/parents
                                                        {:sme/Expression {}}
                                                        :sme/functor
                                                        {:temperature {}}}
       :flow-Coffee-Icecube-Heat-Bar                   {:e1
                                                        {:Coffee
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :e2
                                                        {:Icecube
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :e3
                                                        {:Heat
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :e4
                                                        {:Bar
                                                         {:sme/concept-graph
                                                          :simple-heat-flow}}
                                                        :mops/parents
                                                        {:sme/Expression {}}
                                                        :sme/functor {:flow
                                                                      {}}}}
      (-> {}
          dm/make-mop-graph
          (sut/add-concept-graph :simple-heat-flow
            [:flow :Coffee :Icecube :Heat :Bar]
            [:greater
             [:temperature :Coffee]
             [:temperature :Icecube]]
            [:flat-top :Coffee]
            [:liquid :Coffee])
          sme-dm/standardize-mops))))))

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
                 (lazy-cat (sut/expressions ds-kg :simple-heat-flow)
                           (sut/expressions ds-kg :simple-water-flow))))))))

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
             (set (sut/direct-children ds-kg [:Beaker :Coffee]))))
      (is (= expected-found-children2
             (set (sut/direct-children ds-kg
                                       [:flow-Beaker-Vial-Water-Pipe
                                        :flow-Coffee-Icecube-Heat-Bar]))))))))
