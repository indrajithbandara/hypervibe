(ns hypervibe.core-test
  (:require [clojure.test :refer :all]
            [hypervibe.core :as hyper]))

(deftest test-target-dir
  (testing "FIXME, I fail."
    (is (= (:targ hyper/dirs)
          "target/"))))


