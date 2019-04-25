(ns dpll.test
  (:require  [clojure.test :as t]
             [dpll.core-test]
             ))


(defn -main
  [& args]
  (t/run-tests 'dpll.core-test)
  )
