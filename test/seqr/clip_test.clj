(ns seqr.test.clip-test
  (:require [clojure.test :refer :all]
            [seqr.clip :refer :all]
            [seqr.interpreters :as interp]
            [seqr.serializers :as ser]))

(deftest test-parse-clip
  (testing "Testing action parsing"
    (let [cl (parse-clip "a b")]
      (is (= (-> (get-in cl [1 1]) first :action) "a"))
      (is (= (-> (get-in cl [1 2]) first :action) "b"))))
  (testing "Testing rest parsing"
    (let [cl (parse-clip "a :1 b | c d e f | g")]
      (is (= (-> (get-in cl [1 1]) first :action) "a"))
      (is (= (-> (get-in cl [1 3]) first :action) "b"))
      (is (= (-> (get-in cl [2 1]) first :action) "c"))
      ;; testing that bar rest is a noop at the end of a measure
      (is (= (-> (get-in cl [3 1]) first :action) "g"))))
  (testing "Testing action arg parsing"
    (let [cl (parse-clip "a {arg val}")]
      (is (= (-> (get-in cl [1 1]) first (get "arg")) "val"))))
  (testing "Testing compound actions"
    (let [cl (parse-clip "[a b {x 1}] c")
          [a b] (get-in cl [1 1])]
      (is (= (:action a) "a"))
      (is (= (:action b) "b"))
      (is (= (get b "x") 1))))
  (testing "Testing compound action args"
    (let [cl (parse-clip "[a b] {x 1} c")
          [a b] (get-in cl [1 1])]
      (is (= (:action a) "a"))
      (is (= (get a "x") 1))
      (is (= (:action b) "b"))
      (is (= (get b "x") 1))))
  (testing "Testing preamble/options parsing, div"
    (let [cl (parse-clip "{:div 3 :args {arg1 val1}} a b c d")
          a (first (get-in cl [1 1]))
          d (first (get-in cl [2 1]))]
      (is (= (:div cl) 3))
      (is (= a {:action "a" :action-str "a" "arg1" "val1"}))
      (is (= (:action d) "d"))))
  (testing "Testing S-expression parsing"
    (let [cl (parse-clip "a (nth [1 2 3] (rand-int 3))")
          action (first (get-in cl [1 2]))]
      (is (= (:action-str action) "(nth [1 2 3] (rand-int 3))"))
      (is (contains? #{1 2 3} (:action (interp/interpret cl action))))))
  (testing "Testing serialization"
    (let [cl (parse-clip "{:args {dur 1}} a b {dur 2} c :1 d")
          [action-positions text] (as-str cl)]
      (is (= (clojure.string/trim text) "{:div 4\n :args {dur 1}}\n\na b {dur 2} c   |\n\nd")))))

(deftest test-dynamic-args
  (let [text "{:args {x (rand-int 4)}} a"
        cl (parse-clip text)
        ac (interp/interpret cl (get-in cl [1 1 0]))]
    (is (int? (get ac "x")))
    (is (= (.trim (second (as-str cl))) text))))

(deftest test-action-vars
  (testing "simple action"
    (is (= "a" (-> "{$a a} $a" parse-clip (get-in [1 1 0 :action])))))

  (testing "fn action"
    (let [cl (parse-clip "{$a '(choose [1 2])'} 1 $a")
          ac (interp/interpret cl (get-in cl [1 2 0]))]
      (is (contains? #{1 2} (:action (interp/interpret cl (get-in cl [1 2 0]))))))))
