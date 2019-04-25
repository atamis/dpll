(ns atamis.dpll-test
  (:require [atamis.dpll :as d]
            [clojure.test :as t]))

(t/deftest satisified-test
  (t/testing "normal operation"
    (t/is (d/satisfied? [[true false false]]))
    (t/is (d/satisfied? [[true false false] [false true false]]))
    (t/is (not (d/satisfied? [[false false false]
                              [false false false]])))
    (t/is (d/satisfied? []))))

(t/deftest all-bound-test
  (t/testing "normal operation"
    (t/is (d/all-bound? [[true false true]]))
    (t/is (not (d/all-bound? [[:a false true]])))
    (t/is (not (d/all-bound? [[:a :b :c]])))
    (t/is (not (d/all-bound? [[true false false]
                              [true false [:not :a]]])))))

(t/deftest unbound-variables-test
  (t/testing "normal operation"
    (t/is #{:a} (d/unbound-variables [[true false :a]]))
    (t/is #{} (d/unbound-variables [[true false true]]))
    (t/is #{:a} (d/unbound-variables [[true false true]
                                      [false false [:not :a]]]))))

(t/deftest assign-variable-test
  (t/testing "normal operation"
    (t/is [[true false false]]
          (d/assign-variable [[:a [:not :a] false]] :a true)))
  (t/testing "bulk operation"
    (t/is (= [[true false false]]
             (d/assign-variables  [[:a [:not :a] :b]] {:a true :b false})))))

(t/deftest unit-clause-test
  (t/testing "normal operation"
    (t/is (not (d/unit-clause? [false false true])))
    (t/is (d/unit-clause? [:a false true]))
    (t/is (not (d/unit-clause? [:a :b true])))))

(t/deftest simplify-test
  (t/testing "normal operation"
    (t/is (= [] (d/simplify [[:a :b true]])))
    (t/is (= [[:a :b]] (d/simplify [[:a :b false]])))
    (t/is (= [[:a :b]] (d/simplify [[:a :b true]
                                    [:a :b false]])))
    (t/is (= [[]] (d/simplify [[]])))))

(t/deftest unit-propagate
  (t/testing "normal operation"
    (t/is (= [[false true false]] (d/unit-propagate [[false :a false]])))
    (t/is (= [[true]] (d/unit-propagate [[[:not :a]]])))
    (t/is (= nil (d/unit-propagate [[:a :b]]))))
  (t/testing "repeatedly unit propagate"
    (t/is (= [[true] [true]]
             (d/repeatedly-unit-propagate [[:a] [:b]])))))

(t/deftest assign-pure-literal-test
  (t/testing "normal operation"
    (t/is (= [[:a true] [[:not :a]]] (d/assign-pure-literals [[:a :b] [[:not :a]]])))
    (t/is (= [[:a true] [[:not :a]]] (d/assign-pure-literals
                                      [[:a [:not :b]] [[:not :a]]])))))

(t/deftest dpll-test
  (t/testing "termination"
    (t/is (d/dpll [[true false false] [false true false]])))
  (t/testing "unsatisfiable"
    (t/is (not (d/dpll [[true false false] []])))
    (t/is (not (d/dpll [[:a] [[:not :a]]]))))
  (t/testing "normal-operation"
    (t/is (d/dpll [[:a :b] [[:not :a] :b]]))))
