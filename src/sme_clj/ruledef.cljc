(ns sme-clj.ruledef
  "Structure mapping matching rule definitions. Contains both basic literal
   similarity rules and macros for defining new rulesets."
  (:require
   [sme-clj.datascript-mops :as sme-dm]
   [sme-clj.ruledef         :as rules]
   [sme-clj.typedef         :as types]
   [tuh8888.clojure-utils   :as util]
   [mops.datascript-mops    :as dm]
   [ont-app.datascript-graph.core :as dsg])
  (:import (ont_app.datascript_graph.core DatascriptGraph)))

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
  [g [b t]]
  (->>
   (dm/query-with-rules
    g
    '{:find  [?b-filler ?t-filler]
      :in    [?b ?t ?arg?]
      :where [(p? ?b [::dsg/id :sme/Expression])
              (p? ?t [::dsg/id :sme/Expression])
              #_(ordered-functor? ?b)
              #_(ordered-functor? ?t)
              (edge ?b ?role ?b-filler)
              (edge ?t ?role ?t-filler)
              [?role ::dsg/id ?role-id]
              [(?arg? ?role-id)]
              (or-join
               [?b-filler ?t-filler]
               (and
                (p? ?b-filler [::dsg/id :sme/Entity])
                (p? ?t-filler [::dsg/id :sme/Entity]))
               (and
                (functor-p? ?b-filler [::dsg/id :sme/Function])
                (functor-p? ?t-filler [::dsg/id :sme/Function])))]}
    sme-dm/ds-rules
    b
    t
    sme-dm/structural-role?)))

;; As in SME, basic analogical matching rules, direct port
(def literal-similarity
  (util/vals-as-keys
   :id
   [(make-rule
     :same-functor
     :filter
     (fn [g [b t :as mh]]
       (if (instance? DatascriptGraph g)
         (-> g
             (dm/query '{:find  [?f]
                         :in    [?b ?t]
                         :where [(edge ?b [::dsg/id :sme/functor] ?f)
                                 (edge ?t [::dsg/id :sme/functor] ?f)]}
                       b
                       t)
             seq)
         (types/same-functor? g mh))))
    (make-rule
     :compatible-args
     :intern
     (fn [kg mh]
       (if (instance? DatascriptGraph kg)
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
         (if (instance? DatascriptGraph kg)
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
  [g [b t]]
  (dm/query-with-rules
   g
   '{:find  [?b-filler ?t-filler]
     :in    [?b ?t ?arg?]
     :where [(p? ?b [::dsg/id :sme/Expression])
             (p? ?t [::dsg/id :sme/Expression])
             #_(ordered-functor? ?b)
             #_(ordered-functor? ?t)
             (edge ?b ?role ?b-filler)
             (edge ?t ?role ?t-filler)
             [?role ::dsg/id ?role-id]
             [(?arg? ?role-id)]
             (or-join
              [?b-filler ?t-filler]
              (and
               (p? ?b-filler [::dsg/id :sme/Entity])
               (p? ?t-filler [::dsg/id :sme/Entity]))
              (and
               (functor-p? ?b-filler [::dsg/id :sme/Function])
               (functor-p? ?t-filler [::dsg/id :sme/Function]))
              (and
               (functor-p? ?b-filler [::dsg/id :sme/Attribute])
               (functor-p? ?t-filler [::dsg/id :sme/Attribute])
               (same-functor? ?b-filler ?t-filler)))]}
   sme-dm/ds-rules
   b
   t
   sme-dm/structural-role?))

(def analogy
  (util/vals-as-keys
   :id
   [(make-rule
     :same-functor
     :filter
     (fn [kg [b t :as mh]]
       (if (instance? DatascriptGraph kg)
         (-> kg
             (dm/query-with-rules '{:find [?f]
                                    :in [?b ?t]
                                    :where
                                    [(edge ?b [::dsg/id :sme/functor] ?f)
                                     (edge ?t [::dsg/id :sme/functor] ?f)
                                     (not (p? ?f [::dsg/id :sme/Attribute]))]}
                                  sme-dm/ds-rules
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
       (if (instance? DatascriptGraph kg)
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
