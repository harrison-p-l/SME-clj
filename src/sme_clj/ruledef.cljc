(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require
   [datascript.core         :as d]
   [sme-clj.datascript-mops :as ds]
   [sme-clj.ruledef         :as rules]
   [sme-clj.typedef         :as types]
   [tuh8888.clojure-utils   :as util])
  (:import (clojure.lang Atom)))

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
   ds/ds-rules
   (d/q
    '{:find  [?b-filler ?t-filler]
      :in    [$ ?b ?t ?arg? %]
      :where [(p? ?b [:mops/id :sme/Expression])
              (p? ?t [:mops/id :sme/Expression])
              #_(ordered-functor? ?b)
              #_(ordered-functor? ?t)
              [?b ?role ?b-filler]
              [?t ?role ?t-filler]
              [(?arg? ?role)]
              (or-join
               [?b-filler ?t-filler]
               (and
                (p? ?b-filler [:mops/id :sme/Entity])
                (p? ?t-filler [:mops/id :sme/Entity]))
               (and
                (functor-p? ?b-filler [:mops/id :sme/Function])
                (functor-p? ?t-filler [:mops/id :sme/Function])))]}
    @conn
    b
    t
    ds/structural-role?)))

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity
  (util/vals-as-keys
   :id
   [(make-rule
     :same-functor
     :filter
     (fn [kg [b t :as mh]]
       (if (instance? Atom kg)
         (->> (d/q '{:find  [?f]
                     :in    [$ ?b ?t]
                     :where [[?b :sme/functor ?f] [?t :sme/functor ?f]]}
                   @kg
                   b
                   t)
              seq)
         (types/same-functor? kg mh))))
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
   ds/ds-rules
   (d/q
    '{:find  [?b-filler ?t-filler]
      :in    [$ ?b ?t ?arg? %]
      :where [(parent? ?b :mops/parents [:mops/id :sme/Expression])
              (parent? ?t :mops/parents [:mops/id :sme/Expression])
              #_(ordered-functor? ?b)
              #_(ordered-functor? ?t)
              [?b ?role ?b-filler]
              [?t ?role ?t-filler]
              [(?arg? ?role)]
              (or-join
               [?b-filler ?t-filler]
               (and
                (parent? ?b-filler :mops/parents [:mops/id :sme/Entity])
                (parent? ?t-filler :mops/parents [:mops/id :sme/Entity]))
               (and
                (functor-p? ?b-filler [:mops/id :sme/Function])
                (functor-p? ?t-filler [:mops/id :sme/Function]))
               (and
                (functor-p? ?b-filler [:mops/id :sme/Attribute])
                (functor-p? ?t-filler [:mops/id :sme/Attribute])
                (same-functor? ?b-filler ?t-filler)))]}
    @conn
    b
    t
    ds/structural-role?)))

(def analogy
  (util/vals-as-keys
   :id
   [(make-rule
     :same-functor
     :filter
     (fn [kg [b t :as mh]]
       (if (instance? Atom kg)
         (->> ds/ds-rules
              (d/q '{:find  [?f]
                     :in    [$ ?b ?t %]
                     :where [[?b :sme/functor ?f]
                             [?t :sme/functor ?f]
                             (not (p? ?f [:mops/id :sme/Attribute]))]}
                   @kg
                   b
                   t)
              seq)
         (try ((every-pred (partial types/same-functor? kg)
                           (partial
                            every?
                            (partial (complement types/attribute-functor?) kg)))
               mh)
              (catch NullPointerException _ (throw (ex-info "" {:mh mh})))))))
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
