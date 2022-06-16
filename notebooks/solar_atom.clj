(ns solar-atom
  (:require
   [mops.records    :as mr]
   [sme-clj.core    :as sme]
   [sme-clj.ruledef :as rules]
   [sme-clj.typedef :as types]
   [ubergraph.core  :as uber]))

(defn map-vals
  [f m]
  (->> m
       (map (juxt key (comp f val)))
       (into {})))

(defn as-graph
  [m & {:keys [include-abstrs?]}]
  (reduce
   (fn [g
        [source
         {:keys [concept-graph]
          :as   mop}]]
     (reduce
      (fn [g [role targets]]
        (reduce (fn [g target]
                  (uber/add-directed-edges g [source target {:label role}]))
                g
                (keys targets)))
      (uber/add-nodes-with-attrs
       g
       [source
        {:color (cond (#{:solar-system} (first (keys concept-graph))) :blue
                      (#{:rutherford-atom} (first (keys concept-graph))) :red
                      (nil? concept-graph) :black
                      :else :purple)}])
      (dissoc mop
              :mops/id
              :mops/inst?        :mops/names
              :sme/concept-graph (when-not include-abstrs? :mops/parents))))
   (uber/multidigraph)
   (:mops m)))

(defn visualize-analogy
  [kg]
  (let [functors       (->> kg
                            mops/mop-ids
                            (filter #(mops/abstr? kg % :sme/Functor)))
        concept-graphs (->> kg
                            mops/mop-ids
                            (filter #(mops/abstr? kg % :sme/ConceptGraph)))
        boring-mops    (-> (mr/make-mop-map)
                           types/initialize-kg
                           :mops
                           keys
                           set
                           (into functors))]
    (-> kg
        (update :mops
          (fn [mops]
            (map-vals
             (fn [mop]
               (->> (dissoc mop :mops/id :mops/inst? :mops/names :sme/functor)
                    (remove (fn [[_ filler]]
                              (some boring-mops
                                    (cond-> filler
                                      (not (coll? filler)) vector))))
                    (into {})))
             (apply dissoc mops (into boring-mops concept-graphs)))))
        as-graph
        uber/viz-graph)))

(def kg
  (as-> (mr/make-mop-map) m
    (types/initialize-kg m)
    (reduce (partial apply types/add-entity)
            m
            [[:Sun :sme/Entity nil]
             [:Planet :sme/Entity nil]
             [:Nucleus :sme/Entity nil]
             [:Electron :sme/Entity nil]])
    (reduce (partial apply types/add-entity)
            m
            [[:mass :sme/Function nil :sme/Entity]
             [:charge :sme/Function nil :sme/Entity]
             [:attracts :sme/Function nil :sme/Entity :sme/Entity]
             [:revolve-around :sme/Function nil :sme/Entity :sme/Entity]
             [:temperature :sme/Function nil :sme/Entity]
             [:gravity :sme/Function nil :sme/Expression :sme/Expression]
             [:opposite-sign :sme/Function nil :sme/Expression
              :sme/Expression]
             [:greater :sme/Relation nil :sme/Entity :sme/Entity]
             [:cause :sme/Relation nil :sme/Expression :sme/Expression]
             [:and
              :sme/Relation
              {:sme/ordered? {false {}}}
              :sme/Expression
              :sme/Expression]])
    (apply types/add-concept-graph
           m
           :solar-system
           (let [attracts    [:attracts :Sun :Planet]
                 mass-sun    [:mass :Sun]
                 mass-planet [:mass :Planet]]
             [[:cause
               [:and [:greater mass-sun mass-planet] attracts]
               [:revolve-around :Planet :Sun]]
              [:greater [:temperature :Sun] [:temperature :Planet]]
              [:cause [:gravity mass-sun mass-planet] attracts]]))
    (apply types/add-concept-graph
           m
           :rutherford-atom
           [[:greater [:mass :Nucleus] [:mass :Electron]]
            [:revolve-around :Electron :Nucleus]
            [:cause
             [:opposite-sign [:charge :Nucleus] [:charge :Electron]]
             [:attracts :Nucleus :Electron]]])
    (mops/infer-hierarchy m)))

;; Visualize the kg
(visualize-analogy kg)

(def gmaps
  (sme/match kg
    :rutherford-atom :solar-system
    rules/analogy))

(def inferences
  (->> gmaps
       (sme/perform-inference kg)
       (sort-by :score)
       (reverse)
       (map :transferred)))

(def new-kg
  (-> (apply types/add-concept-graph kg :new-inferences (first inferences))
      mops/infer-hierarchy))

(visualize-analogy new-kg)

#_(rules/apply-rule
   kg
   (rules/analogy :compatible-args)
   [:cause-gravity-mass-Sun-mass-Planet-attracts-Sun-Planet
    :cause-opposite-sign-charge-Nucleus-charge-Electron-attracts-Nucleus-Electron])
