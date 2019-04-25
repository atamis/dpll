(ns atamis.dpll.test
  (:require  [clojure.test :as t]
             [atamis.dpll.core-test]))

(defn -main
  [& args]
  (t/run-tests 'atamis.dpll.core-test))
