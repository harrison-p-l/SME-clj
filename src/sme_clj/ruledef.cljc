(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require
   [sme-clj.typedef      :as types]
   [sme-clj.util         :refer [vals-as-keys]]
   [datascript.core      :as d]
   [mops.datascript-mops :as ds-mops])
  (:import [clojure.lang Atom]))

;;; Rule definition helpers
(defn apply-rule [kg {:keys [body]} mh] (body kg mh))

(defn make-rule
  [id type body]
  {:id   id
   :type type
   :body (case type
           :filter (fn [kg mh] (when (body kg mh) [mh]))
           body)})

(defn ds-compatible-args
  [conn [b t]]
  (->>
   (d/q
    '{:find [(pull ?b-filler [:mops/id]) (pull ?t-filler [:mops/id])]
      :in [$ ?b ?t ?arg? ?Entity ?Function %]
      :where
      [(parent? ?b :mops/parents [:mops/id :sme/Expression])
       [?b :sme/functor ?b-functor]
       (parent? ?t :mops/parents [:mops/id :sme/Expression])
       [?t :sme/functor ?t-functor]
       #_(or-join
          [?b-functor]
          [?b-functor :sme/ordered? true]
          #_(and
             (not [?b-functor :sme/ordered? false])
             (parent-filler? ?b-functor :mops/parents ?b-p :sme/ordered? true)))
       #_(or-join
          [?t-functor]
          [?t-functor :sme/ordered? true]
          (and
           (not [?t-functor :sme/ordered? false])
           (parent-filler? ?t-functor :mops/parents ?t-p :sme/ordered? true)))
       [?b ?role ?b-filler]
       [?t ?role ?t-filler]
       [(?arg? ?role)]
       (or-join
        [?b-filler ?t-filler ?Entity ?Function]
        (every-entity? ?b-filler ?t-filler ?Entity)
        (every-functor-function? ?b-filler ?t-filler ?Function))]}
    @conn
    [:mops/id b]
    [:mops/id t]
    (fn [role] (not (#{"mops" "sme"} (namespace role))))
    (d/entid @conn [:mops/id :sme/Entity])
    (d/entid @conn [:mops/id :sme/Function])
    (into
     ds-mops/abstr-rules
     '[[(every-entity? ?x ?y ?Entity)
        (and
         (parent? ?x :mops/parents ?Entity)
         (parent? ?y :mops/parents ?Entity))]
       [(every-functor-function? ?b-filler ?t-filler ?Function)
        (and
         [?b-filler :sme/functor ?b-filler-functor]
         (parent? ?b-filler-functor :mops/parents ?Function)
         [?t-filler :sme/functor ?t-filler-functor]
         (parent? ?t-filler-functor :mops/parents ?Function))]]))
   (map (partial map :mops/id))))

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity
  (vals-as-keys
   :id
   [(make-rule :same-functor :filter types/same-functor?)
    (make-rule
     :compatible-args
     :intern
     (fn [kg mh]
       (if (instance? Atom kg)
         (ds-compatible-args kg mh)
         (when (every? (partial types/ordered-functor? kg) mh)
           (let [mhs (->> mh
                          (map (partial types/expression-args kg))
                          (apply types/extract-common-role-fillers kg))]
             (->>
              (for [[base target] mhs
                    b             base
                    t             target
                    :let          [mh [b t]]]
                mh)
              (filter
               (some-fn (partial every? (partial types/strict-entity? kg))
                        (partial every?
                                 (partial types/functor-function? kg))))))))))
    ;; this rule not tested much yet
    #_(make-rule
       :commutative-args
       :intern
       (fn [kg [base target :as mh]]
         (if (instance? Atom kg)
           nil
           (when (not-any? (partial types/ordered-functor? kg) mh)
             (for [base   (mapcat second (types/expression-args kg base))
                   target (mapcat second (types/expression-args kg target))
                   :let   [mh [base target]]
                   :when  ((some-fn (partial every?
                                             (partial types/strict-entity? kg))
                                    (partial every?
                                             (partial types/functor-function?
                                                      kg)))
                           mh)]
               mh)))))]))

(defn ds-analogy-compatible-args
  [conn [b t]]
  (->>
   (d/q
    '{:find [(pull ?b-filler [:mops/id])
             (pull ?t-filler [:mops/id])
             #_(pull ?ff [:mops/id])]
      :in [$ ?b ?t ?arg? ?Entity ?Function ?Attribute %]
      :where
      [(parent? ?b :mops/parents [:mops/id :sme/Expression])
       [?b :sme/functor ?b-functor]
       (parent? ?t :mops/parents [:mops/id :sme/Expression])
       [?t :sme/functor ?t-functor]
       #_(or-join
          [?b-functor]
          [?b-functor :sme/ordered? true]
          #_(and
             (not [?b-functor :sme/ordered? false])
             (parent-filler? ?b-functor :mops/parents ?b-p :sme/ordered? true)))
       #_(or-join
          [?t-functor]
          [?t-functor :sme/ordered? true]
          (and
           (not [?t-functor :sme/ordered? false])
           (parent-filler? ?t-functor :mops/parents ?t-p :sme/ordered? true)))
       [?b ?role ?b-filler]
       [?t ?role ?t-filler]
       [(?arg? ?role)]
       (or-join
        [?b-filler ?t-filler ?Entity ?Function ?Attribute]
        (every-entity? ?b-filler ?t-filler ?Entity)
        (every-functor-function? ?b-filler ?t-filler ?Function)
        (every-same-attribute-functor? ?b-filler ?t-filler ?Attribute))]}
    @conn
    [:mops/id b]
    [:mops/id t]
    (fn [role] (not (#{"mops" "sme"} (namespace role))))
    (d/entid @conn [:mops/id :sme/Entity])
    (d/entid @conn [:mops/id :sme/Function])
    (d/entid @conn [:mops/id :sme/Attribute])
    (into
     ds-mops/abstr-rules
     '[[(every-entity? ?x ?y ?Entity)
        (and
         (parent? ?x :mops/parents ?Entity)
         (parent? ?y :mops/parents ?Entity))]
       [(every-functor-function? ?b-filler ?t-filler ?Function)
        (and
         [?b-filler :sme/functor ?b-filler-functor]
         (parent? ?b-filler-functor :mops/parents ?Function)
         [?t-filler :sme/functor ?t-filler-functor]
         (parent? ?t-filler-functor :mops/parents ?Function))]
       [(every-same-attribute-functor? ?b-filler ?t-filler ?Attribute)
        [?b-filler :sme/functor ?filler-functor]
        [?t-filler :sme/functor ?filler-functor]
        (parent? ?filler-functor :mops/parents ?Attribute)]]))
   (map (partial map :mops/id))))

(def analogy
  (vals-as-keys
   :id
   [(make-rule
     :same-functor
     :filter
     (fn [kg mh]
       (try
         ((every-pred
           (partial types/same-functor? kg)
           (partial every? (partial (complement types/attribute-functor?) kg)))
          mh)
         (catch NullPointerException _ (throw (ex-info "" {:mh mh}))))))
    (make-rule
     :compatible-args
     :intern
     (fn [kg mh]
       (if (instance? Atom kg)
         (ds-analogy-compatible-args kg mh)
         (when (every? (partial types/ordered-functor? kg) mh)
           (let [mhs (->> mh
                          (map (partial types/expression-args kg))
                          (apply types/extract-common-role-fillers kg))]
             (->>
              (for [[base target] mhs
                    b             base
                    t             target
                    :let          [mh [b t]]]
                mh)
              (filter
               (some-fn (partial every? (partial types/strict-entity? kg))
                        (partial every? (partial types/functor-function? kg))
                        (every-pred
                         (partial every? (partial types/attribute-functor? kg))
                         (partial types/same-functor? kg))))))))))
    ;; this rule not tested much yet
    #_(make-rule
       :commutative-args
       :intern
       (fn [kg [base target :as mh]]
         (when (not-any? (partial types/ordered-functor? kg) mh)
           (for [base   (types/expression-args kg base)
                 target (types/expression-args kg target)
                 :let   [mh [base target]]
                 :when  ((some-fn
                          (partial every? (partial types/strict-entity? kg))
                          (partial every? (partial types/functor-function? kg))
                          (every-pred (partial every?
                                               (partial types/attribute-functor?
                                                        kg))
                                      (partial types/same-functor? kg)))
                         mh)]
             mh))))]))

; LocalWords:  mh
