(ns dpll.core
  (:require [clojure.set :as set]))

(defn satisfied?
  [cfn]
  (every? #(some identity %) cfn))

(defn all-bound?
  [cfn]
  (every? boolean? (flatten cfn)))

(def cfn
  [[:a :b :c]
   [[:not :a] :b :c]])

(defn unbound-variables
  [cfn]
  (->> cfn
       (map #(map (fn [x] (if (vector? x) (second x) x)) %))
       flatten
       (filter keyword?)
       (into #{})))

(defn assign-variable
  [cfn variable value]
  (map #(map
         (fn [term]
           (condp = term
             variable value
             [:not variable] (not value)
             term term))
         %) cfn))

(defn assign-variables
  [cfn variables]
  (reduce
   (fn [cfn [name value]]
     (assign-variable cfn name value))
   cfn
   variables))

(defn unit-clause?
  [clause]
  (= 1 (count (unbound-variables [clause]))))

(defn unit-propagate
  [cfn]
  (let [unit-clauses (filter unit-clause? cfn)]
    (when (not (empty? unit-clauses))
      (let [unit-clause (rand-nth (into [] unit-clauses))
            unit-variable (first (filter (complement boolean?)
                                         unit-clause))
            value (if (vector? unit-variable) false true)
            name (if (vector? unit-variable)
                   (second unit-variable)
                   unit-variable)]
        (assign-variable cfn name value)))))

(defn repeatedly-unit-propagate
  [cfn]
  (loop [cfn cfn]
    (if-let [cfn-p (unit-propagate cfn)]
      (recur cfn-p)
      cfn)))

(defn assign-pure-literals
  [cfn]
  (let [var-clauses (->> cfn
                         (mapcat identity)
                         (filter (complement boolean?))
                         (into #{}))
        normal-vars (->> var-clauses
                         (filter (complement vector?))
                         (into #{}))
        inverted-vars (->> var-clauses
                           (filter vector?)
                           (map second)
                           (into #{}))
        normal-pures (set/difference normal-vars inverted-vars)
        inverted-pures (set/difference inverted-vars normal-vars)]
    (assign-variables cfn (-> {}
                              (into (map (fn [v] [v true])
                                         normal-pures))
                              (into (map (fn [v] [v false])
                                         inverted-pures))))))

(defn simplify
  [cfn]
  (->> cfn
       (filter (fn [clause] (not (some #(= % true) clause))))
       (map #(filter (complement boolean?) %))))

(defn dpll
  [cfn]
  (let [cfn (-> cfn
                (repeatedly-unit-propagate)
                (assign-pure-literals)
                (simplify))]

    (cond
      (and (all-bound? cfn) (satisfied? cfn)) true
      (some empty? cfn) false
      true
      (let [variables (unbound-variables cfn)
            variable (rand-nth (into [] variables))]
        (or (dpll (assign-variable cfn variable true))
            (dpll (assign-variable cfn variable false)))))))
