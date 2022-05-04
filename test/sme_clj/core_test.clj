(ns sme-clj.core-test
  (:require
   [clojure.test              :refer [testing deftest is]]
   [sme-clj.core              :as sut]
   [sme-clj.ruledef           :as rules]
   [sme-clj.simple-water-heat :refer [kg mops-kg ds-kg]]
   [taoensso.timbre           :as log]
   [sme-clj.util              :as util]
   [datascript.core           :as d]))

(def expected-match-hypotheses
  #{[:Water :Heat]
    [:Water :Coffee]
    [:flat-top-Water :flat-top-Coffee]
    [:liquid-Water :liquid-Coffee]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:diameter-Beaker :temperature-Coffee]
    [:diameter-Vial :temperature-Icecube]
    [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:Beaker :Coffee]
    [:Pipe :Bar]
    [:pressure-Beaker :temperature-Coffee]
    [:pressure-Vial :temperature-Icecube]
    [:Vial :Icecube]})

(deftest create-match-hypotheses-test
  (log/with-level :warn
   ;; Water flow is the base heat flow the target
   (testing "Predicate calculus"
    (is (= expected-match-hypotheses
           (sut/create-match-hypotheses kg
                                        :simple-water-flow
                                        :simple-heat-flow
                                        rules/literal-similarity))))
   (testing "Mops"
    (is (= expected-match-hypotheses
           (sut/create-match-hypotheses mops-kg
                                        :simple-water-flow
                                        :simple-heat-flow
                                        rules/literal-similarity))))
   (testing "Datascript"
    (is (= expected-match-hypotheses
           (->> (sut/create-match-hypotheses ds-kg
                                             :simple-water-flow
                                             :simple-heat-flow
                                             rules/literal-similarity)
                (util/mhs->labels ds-kg)))))))

(def expected-found-roots
  #{[:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
    [:greater-diameter-Beaker-diameter-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:liquid-Water :liquid-Coffee]
    [:flat-top-Water :flat-top-Coffee]})

(deftest find-roots-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is (= expected-found-roots
           (set (sut/find-roots kg expected-match-hypotheses)))))
   (testing "Mops"
    (is (= expected-found-roots
           (set (sut/find-roots mops-kg expected-match-hypotheses)))))
   (testing "Datascript"
    (is (= expected-found-roots
           (->> expected-match-hypotheses
                (util/mhs->ids ds-kg)
                (sut/find-roots ds-kg)
                (util/mhs->labels ds-kg)
                set))))))

(def diameter-gmap
  #{[:greater-diameter-Beaker-diameter-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:diameter-Beaker :temperature-Coffee]
    [:diameter-Vial :temperature-Icecube]
    [:Beaker :Coffee]
    [:Vial :Icecube]})
(def flow-gmap
  #{[:Water :Heat]
    [:Beaker :Coffee]
    [:Pipe :Bar]
    [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
    [:Vial :Icecube]})

(def pressure-gmap
  #{[:Beaker :Coffee]
    [:greater-pressure-Beaker-pressure-Vial
     :greater-temperature-Coffee-temperature-Icecube]
    [:Vial :Icecube]
    [:pressure-Beaker :temperature-Coffee]
    [:pressure-Vial :temperature-Icecube]})

(def liquid-gmap #{[:Water :Coffee] [:liquid-Water :liquid-Coffee]})
(def flat-top-gmap #{[:Water :Coffee] [:flat-top-Water :flat-top-Coffee]})

(def expected-computed-initial-gmaps
  [pressure-gmap flow-gmap flat-top-gmap liquid-gmap diameter-gmap])

(deftest split-into-mhs-sets-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is (= expected-computed-initial-gmaps
           (sut/split-into-mhs-sets kg expected-match-hypotheses))))
   (testing "Mops"
    (is (= expected-computed-initial-gmaps
           (sut/split-into-mhs-sets mops-kg expected-match-hypotheses))))
   (testing "Datascript"
    (is (= (set expected-computed-initial-gmaps)
           (->> expected-match-hypotheses
                (util/mhs->ids ds-kg)
                (sut/split-into-mhs-sets ds-kg)
                (map (partial util/mhs->labels ds-kg))
                set))))))

(def expected-combined-gmaps
  [[pressure-gmap flow-gmap]
   [flow-gmap diameter-gmap]
   [flat-top-gmap liquid-gmap]])

(deftest concistent-combs-of-mhs-sets-test
  (log/with-level :warn
   (testing "Predicate calculus, Mops, Datascript"
    (is
     (= expected-combined-gmaps
        (sut/consistent-combs-of-mhs-sets expected-match-hypotheses
                                          expected-computed-initial-gmaps))))))

(def expected-merged-gmaps
  [#{[:Beaker :Coffee]
     [:Water :Heat]
     [:pressure-Beaker :temperature-Coffee]
     [:Pipe :Bar]
     [:greater-pressure-Beaker-pressure-Vial
      :greater-temperature-Coffee-temperature-Icecube]
     [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
     [:Vial :Icecube]
     [:pressure-Vial :temperature-Icecube]}
   #{[:Beaker :Coffee]
     [:diameter-Vial :temperature-Icecube]
     [:Water :Heat]
     [:Pipe :Bar]
     [:diameter-Beaker :temperature-Coffee]
     [:greater-diameter-Beaker-diameter-Vial
      :greater-temperature-Coffee-temperature-Icecube]
     [:flow-Beaker-Vial-Water-Pipe :flow-Coffee-Icecube-Heat-Bar]
     [:Vial :Icecube]}
   #{[:Water :Coffee]
     [:liquid-Water :liquid-Coffee]
     [:flat-top-Water :flat-top-Coffee]}])

(deftest merge-mhs-sets-test
  (log/with-level :warn
   (testing "Predicate calculus, Mops, Datascript"
    (is (= expected-merged-gmaps
           (sut/merge-mhs-sets expected-combined-gmaps))))))

(def expected-finalized-gmaps
  (map (fn [mhs score] (assoc score :mhs mhs))
       expected-merged-gmaps
       [{:score        18
         :emap-matches 0
         :mapping      {:base   :simple-water-flow
                        :target :simple-heat-flow}}
        {:score        18
         :emap-matches 0
         :mapping      {:base   :simple-water-flow
                        :target :simple-heat-flow}}
        {:score        5
         :emap-matches 0
         :mapping      {:base   :simple-water-flow
                        :target :simple-heat-flow}}]))

(deftest finalize-gmaps-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is (= expected-finalized-gmaps
           (sut/finalize-gmaps kg
                               :simple-water-flow
                               :simple-heat-flow
                               expected-merged-gmaps))))
   (testing "Mops"
    (is (= expected-finalized-gmaps
           (sut/finalize-gmaps mops-kg
                               :simple-water-flow
                               :simple-heat-flow
                               expected-merged-gmaps))))
   (testing "Datascript"
    (is (= expected-finalized-gmaps
           (->> expected-merged-gmaps
                (map (partial util/mhs->ids ds-kg))
                (sut/finalize-gmaps ds-kg :simple-water-flow :simple-heat-flow)
                (map #(update % :mhs (partial util/mhs->labels ds-kg)))))))))

(def expected-generated-inferences
  [#{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe}
   #{:cause-greater-pressure-Beaker-pressure-Vial-flow-Beaker-Vial-Water-Pipe
     :pressure-Vial :pressure-Beaker :greater-pressure-Beaker-pressure-Vial}
   #{}])

(deftest generate-inferences-test
  (log/with-level :warn
   #_(types/ancestor? ds-kg
                      #{}
                      (d/entid @ds-kg [:mops/id :flow-Beaker-Vial-Water-Pipe]))
   #_(testing "Predicate calculus"
      (is (= expected-generated-inferences
             (->> expected-finalized-gmaps
                  (sut/generate-inferences kg)
                  (map :inferences)))))
   #_(testing "Mops"
      (is (= expected-generated-inferences
             (->> expected-finalized-gmaps
                  (sut/generate-inferences mops-kg)
                  (map :inferences)))))
   (testing "Datascript"
    (is (= (set expected-generated-inferences)
           (->> expected-finalized-gmaps
                (map #(update % :mhs (partial util/mhs->ids ds-kg)))
                (sut/generate-inferences ds-kg)
                (map :inferences)
                (util/mhs->labels ds-kg)
                (map set)
                set))))))

(def expected-transferred-inferences
  [#{[:cause :greater-temperature-Coffee-temperature-Icecube
      :flow-Coffee-Icecube-Heat-Bar]}
   #{[:greater [:pressure :Coffee] [:pressure :Icecube]]
     [:pressure :Icecube]
     [:pressure :Coffee]
     [:cause
      [:greater [:pressure :Coffee] [:pressure :Icecube]]
      :flow-Coffee-Icecube-Heat-Bar]}
   #{}])

(defn gmaps-mhs->ids
  [conn gmaps]
  (map
   #(update % :mhs
      (partial
       map
       (partial mapv (comp (partial d/entid @conn) (partial vector :mops/id)))))
   gmaps))

(deftest transfer-inferences-test
  (log/with-level :warn
   #_(testing "Predicate calculus"
      (is (= expected-transferred-inferences
             (->> expected-generated-inferences
                  (map #(assoc %1 :inferences %2) expected-finalized-gmaps)
                  (sut/transfer-inferences kg)
                  (map :transferred)))))
   #_(testing "Mops"
      (is (= expected-transferred-inferences
             (->> expected-generated-inferences
                  (map #(assoc %1 :inferences %2) expected-finalized-gmaps)
                  (sut/transfer-inferences mops-kg)
                  (map :transferred)))))
   (testing "Datascript"
    (is (= expected-transferred-inferences
           (->> expected-generated-inferences
                (map (partial map
                              (comp (partial d/entid @ds-kg)
                                    (partial vector :mops/id))))
                (map #(assoc %1 :inferences %2)
                     (gmaps-mhs->ids ds-kg expected-finalized-gmaps))
                (sut/transfer-inferences ds-kg)
                (map :transferred)
                (util/inferences->labels ds-kg)))))))

(deftest perform-inference-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is (= expected-transferred-inferences
           (->> expected-finalized-gmaps
                (sut/perform-inference kg)
                (map :transferred)))))
   (testing "Mops"
    (is (= expected-transferred-inferences
           (->> expected-finalized-gmaps
                (sut/perform-inference mops-kg)
                (map :transferred)))))
   (testing "Datascript"
    (is (= expected-transferred-inferences
           (->> expected-finalized-gmaps
                (map #(update % :mhs
                        (partial map
                                 (partial mapv
                                          (comp (partial d/entid @ds-kg)
                                                (partial vector :mops/id))))))
                (sut/perform-inference ds-kg)
                (map :transferred)
                (util/inferences->labels ds-kg)))))))

(deftest match-test
  (log/with-level :warn
   (testing "Predicate calculus"
    (is (= expected-finalized-gmaps
           (sut/match kg
             :simple-water-flow :simple-heat-flow)))
    (is (= expected-finalized-gmaps
           (sut/match kg
             :simple-water-flow :simple-heat-flow
             rules/literal-similarity))))
   (testing "Mops"
    (is (= expected-finalized-gmaps
           (sut/match mops-kg
             :simple-water-flow :simple-heat-flow))))
   (testing "Datascript"
    (is (= (set expected-finalized-gmaps)
           (-> ds-kg
               (sut/match :simple-water-flow :simple-heat-flow)
               (->> (map #(update % :mhs (partial util/mhs->labels ds-kg)))
                    set)))))))

; LocalWords:  gmaps
