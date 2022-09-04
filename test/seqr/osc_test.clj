(ns seqr.test.osc
  (:require [clojure.test :refer :all]
            [seqr.osc :as osc]))

(deftest test-osc-builder
  (testing "Testing osc message building"
    (let [template "/url 123 ?var ?defaulted:val"
          builder (osc/builder template)
          msg (into [] (builder {:var 1}))]
      (is (= msg [47 117 114 108 0 0 0 0 44 105 105 115 0 0 0 0 0 0 0 123 0 0 0 1 118 97 108 0])))))
