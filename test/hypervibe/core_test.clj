(ns hypervibe.core-test
  (:require [clojure.test :refer :all]
            [hypervibe.core :as hyper]))

(deftest test-target-dir
  (testing
    "Hypervibe artifact target directory"
    (is (= (:targ hyper/dirs)
          "target/"))))

(deftest test-template-files
  (testing "Hypervibe template file"
    (is (= (:hyper hyper/files)
          "hypervibe")
      (= (:hyper-pack hyper/files)
        "hypervibe-packaged"))))

(deftest test-file-extensions
  (testing "FIXME, I fail."
    (is (and (= (:edn hyper/exten)
               ".edn")
          (= (:json hyper/exten)
            ".json")))))


